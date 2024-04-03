---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:13.255029-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E08\u0E32\u0E01\u0E44\
  \u0E1F\u0E25\u0E4C .txt \u0E1A\u0E19\u0E14\u0E34\u0E2A\u0E01\u0E4C\u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\
  \u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32\
  , \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49,\u2026"
lastmod: '2024-03-17T21:57:56.012479-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E08\u0E32\u0E01\u0E44\
  \u0E1F\u0E25\u0E4C ."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## อะไรและทำไม?
การอ่านไฟล์ข้อความคือการเรียกข้อความจากไฟล์ .txt บนดิสก์ของคุณ โปรแกรมเมอร์ทำเช่นนี้เพื่อจัดการข้อมูลเช่นการกำหนดค่า, ข้อมูลผู้ใช้, หรือเพื่อประมวลผลข้อความจำนวนมาก

## วิธีการ:
ไลบรารีมาตรฐานของ Rust ทำให้การอ่านไฟล์เป็นเรื่องง่าย

```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("example.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("เนื้อหาของไฟล์:\n{}", contents);
    Ok(())
}
```
โค้ดนี้เปิด "example.txt", อ่านมัน, และพิมพ์เนื้อหาออกมา

ตัวอย่างผลลัพธ์:
```
เนื้อหาของไฟล์:
สวัสดี, Rustaceans!
```

## การดำดิ่งลึก
ในอดีต, การจัดการไฟล์ IO อาจซับซ้อน แต่ Rust ทำให้มันง่ายขึ้น มีทางเลือกอื่นที่ ‘read_to_string’ เช่นการใช้ ‘BufRead’ เพื่อการจัดการทีละบรรทัด ซึ่งมีประสิทธิภาพมากขึ้นกับไฟล์ขนาดใหญ่ ในเบื้องหลัง, การอ่านไฟล์ของ Rust ใช้ประโยชน์จากการเรียกระบบระดับ OS, บัฟเฟอร์ข้อมูลเพื่อประสิทธิภาพ

หลังจาก Rust 1.0, ภาษาเน้นการมีปฏิสัมพันธ์กับระบบที่ปลอดภัย - การอ่านไฟล์ก็ไม่ใช่ข้อยกเว้น ประเภท ‘Result’ ปกปิดข้อผิดพลาดที่อาจเกิดขึ้น, ทำให้ Rust มีความทนทานต่อข้อผิดพลาดทั่วไปเช่นไฟล์ที่หายไปหรือปัญหาการอนุญาตโดยไม่ต้องใช้ป๊านิค

## ดูเพิ่มเติม
ทรัพยากรเพิ่มเติมให้ตรวจสอบ:
- เอกสารของ Rust เกี่ยวกับไฟล์ I/O: [std::fs](https://doc.rust-lang.org/std/fs/)
- บทของหนังสือเกี่ยวกับการจัดการข้อผิดพลาด: [การจัดการข้อผิดพลาด](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- Rust by Example เกี่ยวกับไฟล์ I/O: [ไฟล์ I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
