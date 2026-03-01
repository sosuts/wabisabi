// 標準ライブラリを使わず、エントリポイントを自分で定義することを示。
// UEFIのようなOS外環境で必要
#![no_main]
#![no_std]

use core::fmt::Write;
use core::panic::PanicInfo;
use core::writeln;
use wabisabi::graphics::{draw_test_pattern, fill_rect, Bitmap};
use wabisabi::qemu::{exit_qemu, QemuExitCode};
use wabisabi::serial::SerialPort;
use wabisabi::uefi::{
    init_vram, EfiHandle, EfiMemoryType, EfiStatus, EfiSystemTable, MemoryMapHolder, VramTextWriter,
};
use wabisabi::x86::hlt;

#[no_mangle]
fn efi_main(_image_handle: EfiHandle, efi_system_table: &EfiSystemTable) {
    let mut sw = SerialPort::new_for_com1();
    writeln!(sw, "Hello via serial port").unwrap();
    let mut vram = init_vram(efi_system_table).expect("init_vram failed");
    let vw = vram.width();
    let vh = vram.height();
    fill_rect(&mut vram, 0x000000, 0, 0, vw, vh).expect("fill_rect failed");
    draw_test_pattern(&mut vram);

    let mut w = VramTextWriter::new(&mut vram);
    for i in 0..4 {
        writeln!(w, "i = {}", i).unwrap();
    }
    let mut memory_map = MemoryMapHolder::new();
    let status = efi_system_table
        .boot_services
        .get_memory_map(&mut memory_map);
    writeln!(w, "{status:?}").unwrap();
    let mut total_memory_pages = 0;
    for e in memory_map.iter() {
        if e.memory_type != EfiMemoryType::CONVENTIONAL_MEMORY {
            continue;
        }
        total_memory_pages += e.number_of_pages;
        writeln!(w, "{e:?}",).unwrap();
    }
    let total_memory_size_mib = total_memory_pages * 44096 / 1024 / 1024;
    writeln!(
        w,
        "Total: {total_memory_pages} pages = {total_memory_size_mib} MiB"
    )
    .unwrap();
    exit_from_efi_boot_services(_image_handle, efi_system_table, &mut memory_map);
    writeln!(w, "Hello, Non-UEFI world").unwrap();
    loop {
        hlt()
    }
}

fn exit_from_efi_boot_services(
    image_handle: EfiHandle,
    efi_system_table: &EfiSystemTable,
    memory_map: &mut MemoryMapHolder,
) {
    loop {
        let status = efi_system_table.boot_services.get_memory_map(memory_map);
        assert_eq!(status, EfiStatus::Success);
        let status =
            (efi_system_table.boot_services.exit_boot_services)(image_handle, memory_map.map_key);
        if status == EfiStatus::Success {
            break;
        }
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    exit_qemu(QemuExitCode::Fail);
    loop {}
}
