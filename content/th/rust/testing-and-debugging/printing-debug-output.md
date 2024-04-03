---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:34.733511-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E2B\u0E32\u0E01\u0E15\
  \u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E2A\u0E34\u0E48\
  \u0E07\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E43\u0E2B\u0E49\u0E43\u0E0A\
  \u0E49 `println!` \u0E2B\u0E32\u0E01\u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E04\u0E48\u0E32\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E1B\u0E31\u0E0D\u0E2B\u0E32\
  \ `dbg!` \u0E01\u0E47\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\
  ."
lastmod: '2024-03-17T21:57:55.996290-06:00'
model: gpt-4-0125-preview
summary: "\u0E2B\u0E32\u0E01\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E1E\u0E34\
  \u0E21\u0E1E\u0E4C\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\
  \u0E46 \u0E43\u0E2B\u0E49\u0E43\u0E0A\u0E49 `println!` \u0E2B\u0E32\u0E01\u0E04\u0E38\
  \u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E04\
  \u0E48\u0E32\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\
  \u0E02\u0E1B\u0E31\u0E0D\u0E2B\u0E32 `dbg!` \u0E01\u0E47\u0E21\u0E35\u0E1B\u0E23\
  \u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
หากต้องการพิมพ์สิ่งที่ง่ายๆ ให้ใช้ `println!` หากคุณต้องการพิมพ์ค่าเพื่อการแก้ไขปัญหา `dbg!` ก็มีประโยชน์

```Rust
fn main() {
    let mut vec = vec![1, 2, 3];
    
    // การพิมพ์ข้อมูลพื้นฐาน
    println!("Hello, Rustaceans!");

    // การกำหนดรูปแบบการแก้ไขปัญหาด้วย println! โดยใช้ `{:?}`
    println!("{:?}", vec);

    // การแก้ไขปัญหาด้วย `dbg!`, พิมพ์ไปยัง stderr และคืนค่ากลับ
    dbg!(&vec);

    // การแก้ไข vec หลังจากใช้ `dbg!`
    vec.push(4);
    dbg!(vec);
}
```

ตัวอย่างผลลัพธ์:

```
Hello, Rustaceans!
[1, 2, 3]
[src/main.rs:9] &vec = [
    1,
    2,
    3,
]
[src/main.rs:13] vec = [
    1,
    2,
    3,
    4,
]
```

## การศึกษาอย่างลึกซึ้ง
การพิมพ์ข้อมูลเพื่อแก้ปัญหาเป็นส่วนหนึ่งที่ตรงไปตรงมาของการเขียนโปรแกรมตั้งแต่ยุคแรกเริ่ม ความเรียบง่ายของมันมักทำให้เป็นตัวเลือกแรกๆ สำหรับการวินิจฉัยปัญหาอย่างรวดเร็ว

ใน Rust, `println!` เหมาะกับการแสดงข้อความที่เป็นมิตรกับผู้ใช้ ความวิเศษมากับ `dbg!`, ซึ่งเปิดตัวใน Rust 1.32, ซึ่งพิมพ์ทั้งค่าและตำแหน่งของมันในโค้ด มันส่งออกผ่าน standard error (stderr), จึงไม่จะผสมกับ standard output (stdout) และสามารถเปลี่ยนทิศทางได้แยกต่างหากหากจำเป็น

สำหรับประเภทที่ซับซ้อน, คุณสามารถสืบทอด trait `Debug` เพื่อสร้างรูปแบบอัตโนมัติที่ `println!` และ `dbg!` สามารถใช้ได้ นั่นคือสิ่งที่การประกาศ `#[derive(Debug)]` ทำเหนือ structs และ enums ของคุณ

ในฐานะทางเลือกอื่น, โปรแกรมบันทึกข้อมูลที่เหมาะสมอย่าง `log` และ `env_logger` มีอยู่, และหากคุณต้องการควบคุมในแบบเฉพาะเจาะจง, พิจารณาใช้เดบักเกอร์เช่น `gdb` หรือ `lldb`, ซึ่งทำงานกับ Rust ผ่านการรวมเข้าด้วยกันเช่น `rust-gdb` หรือ `rust-lldb`

## ดูเพิ่มเติม
สำหรับข้อมูลเพิ่มเติมเกี่ยวกับตัวเลือกการพิมพ์และรูปแบบการแก้ไขปัญหาใน Rust:

- หนังสือ Rust เกี่ยวกับ `println!` และการกำหนดรูปแบบ: https://doc.rust-lang.org/std/fmt/index.html
- เอกสารของแมโคร `dbg!`: https://doc.rust-lang.org/std/macro.dbg.html
- คู่มือทางการสำหรับการแก้ไขปัญหาด้วย `gdb` และ `lldb`: https://rust-lang.github.io/rustup-components-history
- ตู้คอนเทนเนอร์ `log` สำหรับการเข้าใกล้การบันทึกข้อมูลที่มีโครงสร้างมากขึ้น: https://crates.io/crates/log
- ตู้คอนเทนเนอร์ `env_logger`, การบันทึกที่เป็นที่นิยมสำหรับฟาซาด `log`: https://crates.io/crates/env_logger
