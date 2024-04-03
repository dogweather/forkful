---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:46.385401-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Rust \u0E23\u0E2D\u0E07\u0E23\
  \u0E31\u0E1A\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E2B\u0E25\
  \u0E32\u0E22\u0E15\u0E31\u0E27 \u0E41\u0E15\u0E48\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\
  \u0E48\u0E2D\u0E22\u0E04\u0E37\u0E2D `gdb` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \ GNU/Linux \u0E2B\u0E23\u0E37\u0E2D `lldb` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \ macOS \u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E08\u0E30\u0E43\u0E0A\u0E49 `rust-gdb`\
  \ \u0E2B\u0E23\u0E37\u0E2D `rust-lldb` \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u2026"
lastmod: '2024-03-17T21:57:55.998481-06:00'
model: gpt-4-0125-preview
summary: "Rust \u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E2B\u0E25\u0E32\u0E22\u0E15\u0E31\u0E27 \u0E41\u0E15\u0E48\
  \u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\u0E22\u0E04\u0E37\u0E2D `gdb` \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A GNU/Linux \u0E2B\u0E23\u0E37\u0E2D `lldb` \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A macOS \u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E08\u0E30\u0E43\
  \u0E0A\u0E49 `rust-gdb` \u0E2B\u0E23\u0E37\u0E2D `rust-lldb` \u0E0B\u0E36\u0E48\u0E07\
  \u0E40\u0E1B\u0E47\u0E19 wrapper \u0E17\u0E35\u0E48\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E04\u0E48\u0E32\u0E02\u0E2D\u0E07\
  \ Rust \u0E2D\u0E2D\u0E01\u0E21\u0E32\u0E2A\u0E27\u0E22\u0E07\u0E32\u0E21 \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
Rust รองรับดีบักเกอร์หลายตัว แต่ที่พบบ่อยคือ `gdb` สำหรับ GNU/Linux หรือ `lldb` สำหรับ macOS คุณอาจจะใช้ `rust-gdb` หรือ `rust-lldb` ซึ่งเป็น wrapper ที่ทำให้การพิมพ์ค่าของ Rust ออกมาสวยงาม นี่คือตัวอย่าง:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

เพื่อดีบักสิ่งนี้ คอมไพล์พร้อมข้อมูลดีบัก:

```shell
$ rustc -g counter.rs
```

จากนั้นรันมันใน `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## การศึกษาเพิ่มเติม
การดีบักเกิดขึ้นตั้งแต่ *ยุคแรกๆ* ของการ์ดเจาะรู และการพัฒนาของมันเป็นพรอันประเสริฐ Rust มีเครื่องมือของตัวเองพร้อมการรวมกับ GDB และ LLDB เนื่องจากลักษณะของภาษาที่เป็นระดับระบบ

ทางเลือกอื่นสำหรับการดีบักโค้ด Rust รวมถึงการใช้สภาพแวดล้อมการพัฒนาแบบบูรณาการ (IDEs) พร้อมดีบักเกอร์ในตัวซึ่งบางคนพบว่าใช้งานง่ายกว่า ตัวเลือกยอดนิยม ได้แก่ CLion ที่มีปลั๊กอิน Rust หรือ Visual Studio Code ที่มีส่วนขยายของ Rust

สำหรับการนำไปใช้งาน Rust สร้างสัญลักษณ์การดีบักที่ดีบักเกอร์เหล่านี้เข้าใจได้ซึ่งมีความสำคัญสำหรับการเดินผ่านโค้ด การตั้งจุดหยุด และการตรวจสอบตัวแปรโดยไม่สูญเสียสติ

## ดูเพิ่มเติม
- The Rust Book บนการดีบัก: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example สาธิตข้อผิดพลาดและการดีบัก: https://doc.rust-lang.org/rust-by-example/error.html
- The Rust Language Server (RLS) ที่ช่วยขับเคลื่อนส่วนขยายของ Rust ใน VS Code: https://github.com/rust-lang/rls
- การดีบัก Rust ด้วย Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
