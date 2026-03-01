use core::alloc::Layout;
use core::cell::RefCell;
use core::cmp::max;
use core::ops::DerefMut;
use core::ptr::null_mut;
use core::{fmt, panic};

use crate::result::Result;
use crate::uefi::{EfiMemoryDescriptor, EfiMemoryType, MemoryMapHolder};
use alloc::alloc::GlobalAlloc;

extern crate alloc;
use alloc::boxed::Box;

pub fn round_up_to_nearest_pow2(v: usize) -> Result<usize> {
    1usize
        .checked_shl(usize::BITS - v.wrapping_sub(1).leading_zeros())
        .ok_or("Out of range")
}
#[test_case]
fn round_up_to_nearest_pow2_tests() {
    unimplemented!("未実装だから失敗")
}

/// Vertical bar `|`はヘッダーがあるチャンクを示す
/// before: |--prev------|----self-------------------
/// align:  |-----|-----|-------|-------|-----------|
/// after:  |--prev------||-----|-------------------|
struct Header {
    next_header: Option<Box<Header>>,
    // Box<T>は「T型の値をヒープに確保し、所有権を持つ」スマートポインタ
    // Box<Header>は「Header型の値をヒープに置き、所有権を持つ」ことを意味する
    // Boxがスコープを抜けると、自動的にメモリが解放される
    // 値の所有権はBoxが持つので、moveや参照渡しも安全にできる
    size: usize,

    is_allocated: bool,
    _reserved: usize,
}
const HEADER_SIZE: usize = core::mem::size_of::<Header>();

#[allow(clippy::assertions_on_constants)]
const _: () = assert!(HEADER_SIZE == 32, "Header size must be 32 bytes");
// HEADER_SIZEを表すビットが1つだけ立っていることを確認
const _: () = assert!(HEADER_SIZE.count_ones() == 1,);

pub const LAYOUT_PAGE_4K: Layout = unsafe { Layout::from_size_align_unchecked(4096, 4096) };
impl Header {
    fn can_provide(&self, size: usize, align: usize) -> bool {
        self.size >= size + HEADER_SIZE * 2 + align
    }
    fn is_allocated(&self) -> bool {
        self.is_allocated
    }
    fn end_addr(&self) -> usize {
        self as *const Header as usize + self.size
    }

    unsafe fn new_from_addr(addr: usize) -> Box<Header> {
        let header = addr as *mut Header;
        header.write(Header {
            next_header: None,
            size: 0,
            is_allocated: false,
            _reserved: 0,
        });
        Box::from_raw(addr as *mut Header)
    }
    unsafe fn from_allocated_region(addr: *mut u8) -> Box<Header> {
        let header = addr.sub(HEADER_SIZE) as *mut Header;
        Box::from_raw(header)
    }
    fn provide(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        let size = max(round_up_to_nearest_pow2(size).ok()?, HEADER_SIZE);
        let align = max(align, HEADER_SIZE);
        if self.is_allocated() || !self.can_provide(size, align) {
            None
        } else {
            let mut size_used = 0;
            let allocated_addr = (self.end_addr() - size) & !(align - 1);
            let mut header_for_allocated =
                unsafe { Self::new_from_addr(allocated_addr - HEADER_SIZE) };
            header_for_allocated.is_allocated = true;
            header_for_allocated.size = size + HEADER_SIZE;
            size_used += header_for_allocated.size;
            if header_for_allocated.end_addr() != self.end_addr() {
                let mut header_for_padding =
                    unsafe { Self::new_from_addr(header_for_allocated.end_addr()) };
                header_for_padding.is_allocated = false;
                header_for_padding.size = self.end_addr() - header_for_allocated.end_addr();
                size_used += header_for_padding.size;
                header_for_padding.next_header = header_for_allocated.next_header.take();
                header_for_allocated.next_header = Some(header_for_padding);
            }
            assert!(self.size >= size_used + HEADER_SIZE);
            self.size -= size_used;
            self.next_header = Some(header_for_allocated);
            Some(allocated_addr as *mut u8)
        }
    }
}

impl Drop for Header {
    fn drop(&mut self) {
        panic!("Header should not be dropped!")
    }
}

impl fmt::Debug for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Header @ {:#018X} {{ size: {:#018X}, is_allocated: {} }}",
            self as *const Header as usize,
            self.size,
            self.is_allocated()
        )
    }
}

pub struct FirstFitAllocator {
    first_header: RefCell<Option<Box<Header>>>,
}

#[global_allocator]
pub static ALLOCATOR: FirstFitAllocator = FirstFitAllocator {
    first_header: RefCell::new(None),
};

unsafe impl Sync for FirstFitAllocator {}
unsafe impl GlobalAlloc for FirstFitAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.alloc_with_options(layout)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        let mut region = Header::from_allocated_region(ptr);
        region.is_allocated = false;
        Box::leak(region);
        // ここでリークさせるのは、メモリ上のフリー情報をドロップしてしまわないようにするため（わからん）
    }
}

impl FirstFitAllocator {
    pub fn alloc_with_options(&self, layout: Layout) -> *mut u8 {
        let mut header = self.first_header.borrow_mut();
        let mut header = header.deref_mut();
        loop {
            match header {
                Some(e) => match e.provide(layout.size(), layout.align()) {
                    Some(p) => break p,
                    None => {
                        header = &mut e.next_header;
                        continue;
                    }
                },
                None => {
                    break null_mut::<u8>();
                }
            }
        }
    }
    pub fn init_with_mmap(&self, memory_map: &MemoryMapHolder) {
        for e in memory_map.iter() {
            if e.memory_type() != EfiMemoryType::CONVENTIONAL_MEMORY {
                continue;
            }
            self.add_free_from_descriptor(e);
        }
    }
    pub fn add_free_from_descriptor(&self, desc: &EfiMemoryDescriptor) {
        let mut start_addr = desc.physical_start() as usize;
        let mut size = desc.number_of_pages() as usize * 4096;
        // アロケーターがアドレス0をフリーとしてあると誤認識しないようにするため、アドレス0をフリーから除外する
        if start_addr == 0 {
            start_addr += 4096;
            size = size.saturating_sub(4096);
        }
        if size <= 4096 {
            return;
        }
        let mut header = unsafe { Header::new_from_addr(start_addr) };
        header.next_header = None;
        header.is_allocated = false;
        header.size = size;
        let mut first_header = self.first_header.borrow_mut();
        let prev_last = first_header.replace(header);
        drop(first_header);
        let mut header = self.first_header.borrow_mut();
        header.as_mut().unwrap().next_header = prev_last;
        // ここでheadersをソートしていないのはおっけー
    }
}
