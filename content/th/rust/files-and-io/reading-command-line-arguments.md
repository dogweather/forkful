---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:34.631656-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E43\u0E19 Rust \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\
  \u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 \u0E2A\u0E34\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E41\u0E1A\
  \u0E1A\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E40\u0E2D\u0E07\u0E42\u0E14\u0E22\u0E44\u0E21\
  \u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.010084-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E43\u0E19 Rust \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\
  \u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 \u0E2A\u0E34\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E41\u0E1A\
  \u0E1A\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E40\u0E2D\u0E07\u0E42\u0E14\u0E22\u0E44\u0E21\
  \u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49 GUI."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีการ:
นี่คือวิธีที่ง่ายที่สุดในการรับอาร์กิวเมนต์:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

รันด้วยคำสั่ง `cargo run arg1 arg2` คุณจะเห็น:

```
["path/to/executable", "arg1", "arg2"]
```

ตัวเลือกที่เรียบร้อยกว่าด้วยตัวทำซ้ำ:

```Rust
use std::env;

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", arg);
    }
}
```

ตอนนี้ลองใช้ `cargo run cool stuff`:

```
cool
stuff
```

## ศึกษาเพิ่มเติม
โดยประวัติศาสตร์แล้ว อาร์กิวเมนต์บรรทัดคำสั่งเป็นเรื่องย้อนยุคสมัยที่ GUI ไม่แพร่หลาย ปัจจุบัน มันเหมาะมากสำหรับสคริปต์ เซิร์ฟเวอร์ หรือเครื่องมือ

`std::env::args` ของ Rust ใช้ตัวทำซ้ำ (iterator) ซึ่งประหยัดหน่วยความจำและทำงานแบบเกียจคร้าน มันยังจัดการกับ Unicode ได้ด้วย มี `args_os` สำหรับสตริงของระบบปฏิบัติการดิบ

สำหรับการวิเคราะห์ที่ซับซ้อน คลังเช่น `clap` หรือ `structopt` สามารถช่วยได้ พวกเขาวิเคราะห์ธง ตัวเลือก และย่อยคำสั่ง

## ดูเพิ่มเติมที่
- [โมดูล `std::env` ของ Rust](https://doc.rust-lang.org/std/env/)
- [เอกสารคลัง `clap`](https://docs.rs/clap/)
- [หนังสือ Rust เกี่ยวกับอาร์กิวเมนต์บรรทัดคำสั่ง](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
