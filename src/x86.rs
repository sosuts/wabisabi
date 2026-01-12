use core::arch::asm;

// HLT命令を実行する関数
pub fn hlt() {
    unsafe { asm!("hlt") };
}

pub fn write_io_port_u8(port: u16, data: u8) {
    unsafe {
        asm!("out dx, al", in("dx") port, in("al") data);
    }
}
