---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:28.807159-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E43\u0E19 Rust \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E19\u0E33\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\
  \u0E31\u0E22\u0E44\u0E1B\u0E17\u0E35\u0E48\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E41\
  \u0E22\u0E01\u0E15\u0E48\u0E32\u0E07\u0E2B\u0E32\u0E01\u0E08\u0E32\u0E01 standard\
  \ output (stdout)\u2026"
lastmod: '2024-03-17T21:57:56.011110-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E43\u0E19 Rust \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E19\u0E33\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\
  \u0E31\u0E22\u0E44\u0E1B\u0E17\u0E35\u0E48\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E41\
  \u0E22\u0E01\u0E15\u0E48\u0E32\u0E07\u0E2B\u0E32\u0E01\u0E08\u0E32\u0E01 standard\
  \ output (stdout)\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การเขียนไปยัง standard error (stderr) ใน Rust คือการนำข้อความผิดพลาดและการวินิจฉัยไปที่คอนโซลแยกต่างหากจาก standard output (stdout) นักพัฒนาทำเช่นนี้เพื่อแยกความแตกต่างระหว่างผลลัพธ์ปกติของโปรแกรมและข้อความผิดพลาด เพื่อให้สามารถจัดการกับข้อผิดพลาดได้อย่างเหมาะสมหรือเปลี่ยนทิศทางพวกเขาไปยังไฟล์บันทึกหรือไฟล์ระหว่างการดำเนินการ

## วิธีการ:
Rust ให้วิธีการที่ตรงไปตรงมาในการเขียนไปยัง stderr โดยใช้ macro `eprintln!` คล้ายกับวิธีใช้ `println!` สำหรับ stdout นี่คือตัวอย่างพื้นฐาน:

```rust
fn main() {
    eprintln!("This is an error message!");
}
```

ตัวอย่างผลลัพธ์ (ไปยัง standard error):
```
This is an error message!
```

สำหรับการควบคุมข้อความผิดพลาดได้มากขึ้น เช่นเมื่อคุณต้องการจัดรูปแบบข้อความหรือจัดการกับผลลัพธ์ของ I/O ให้ใช้ฟังก์ชัน `stderr` จากโมดูล `std::io` วิธีนี้จะให้ตัวจัดการไปยังกระแส stderr ทั่วโลก ซึ่งคุณสามารถเขียนไปยังมันได้โดยใช้เมท็อด เช่น `write_all` หรือ `writeln` จาก trait `Write`:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Formatted error message: {}", 404).expect("Failed to write to stderr");
}
```

ตัวอย่างผลลัพธ์ (ไปยัง standard error):
```
Formatted error message: 404
```

หากคุณกำลังทำงานในสภาวะแวดล้อมหรือแอปพลิเคชั่นที่คุณพึ่งพาไลบรารีสำหรับการบันทึกข้อมูลหรือการจัดการข้อผิดพลาด ไลบรารีเช่น `log` และ `env_logger` มีความนิยม ถึงแม้ว่าจะถูกใช้เพื่อวัตถุประสงค์การบันทึกข้อมูลมากกว่า แต่สามารถกำหนดค่าได้และสามารถนำระดับข้อผิดพลาดของการบันทึกไปยัง stderr นี่คือตัวอย่างการใช้งานง่ายๆ โดยใช้ `log` และ `env_logger`:

ก่อนอื่น เพิ่มการพึ่งพาใน `Cargo.toml` ของคุณ:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

จากนั้น ตั้งค่าและใช้การบันทึกข้อมูลในแอปพลิเคชั่นของคุณ:
```rust
fn main() {
    env_logger::init();
    log::error!("This is an error message logged to stderr");
}
```

การเรียกใช้โปรแกรมนี้ (หลังจากตั้งค่า `env_logger` ด้วยตัวแปรสภาพแวดล้อมที่เหมาะสม เช่น `RUST_LOG=error`) จะส่งผลให้ข้อความผิดพลาดแสดงใน stderr โดยใช้โครงสร้างพื้นฐานการบันทึกข้อมูล

```plaintext
ERROR: This is an error message logged to stderr
```
