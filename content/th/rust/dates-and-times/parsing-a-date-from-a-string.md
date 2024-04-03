---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:05.187600-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.004421-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## วิธีการ:


### การใช้ไลบรารีมาตรฐานของ Rust (`chrono` Crate)
ไลบรารีมาตรฐานของ Rust ไม่รวมฟังก์ชันการแปลงวันที่โดยตรง แต่ crate `chrono` ที่ใช้กันอย่างแพร่หลาย เป็นโซลูชันที่มั่นคงสำหรับการจัดการวันที่และเวลา ก่อนอื่น เพิ่ม `chrono` ลงใน `Cargo.toml` ของคุณ:

```toml
[dependencies]
chrono = "0.4"
```

จากนั้น ใช้ `chrono` เพื่อแปลงสตริงวันที่เป็นออบเจ็ค `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Failed to parse date");

    println!("วันที่ที่แปลงได้: {}", date);
}

// ตัวอย่างผลลัพธ์:
// วันที่ที่แปลงได้: 2023-04-01
```

### การใช้การจัดการวัน-เวลาขั้นสูงของ Rust (`time` Crate)
สำหรับการจัดการวัน-เวลาที่ซับซ้อนมากขึ้น รวมถึงการแปลงที่สะดวกยิ่งขึ้น พิจารณาใช้ crate `time` ก่อนอื่น รวมมันเข้ากับ `Cargo.toml` ของคุณ:

```toml
[dependencies]
time = "0.3"
```

จากนั้น แปลงสตริงวันที่โดยใช้ประเภท `Date` และ `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Failed to parse date and time");

    println!("วันที่และเวลาที่แปลงได้: {}", parsed_date);
}

// ตัวอย่างผลลัพธ์:
// วันที่และเวลาที่แปลงได้: 2023-04-01 12:34:56
```

ทั้งสองตัวอย่างแสดงให้เห็นว่า Rust กับการช่วยเหลือจาก crates ของบุคคลที่สาม สามารถทำให้งานการแปลงสตริงวันที่เป็นออบเจ็ควันที่ที่สามารถจัดการได้ ทำให้เป็นเครื่องมือที่มีประสิทธิภาพสำหรับการพัฒนาซอฟต์แวร์ที่เกี่ยวข้องกับข้อมูลเชิงเวลา
