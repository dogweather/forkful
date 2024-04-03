---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:39.316166-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Rust \u0E21\u0E35 `chrono`\
  \ crate \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E17\u0E38\u0E01\u0E04\u0E27\u0E32\
  \u0E21\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\
  \u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\
  \u0E25\u0E32 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E25\u0E1A\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48."
lastmod: '2024-03-17T21:57:56.008209-06:00'
model: gpt-4-0125-preview
summary: "Rust \u0E21\u0E35 `chrono` crate \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E17\
  \u0E38\u0E01\u0E04\u0E27\u0E32\u0E21\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E2B\u0E23\u0E37\
  \u0E2D\u0E25\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48."
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## วิธีการ:
Rust มี `chrono` crate สำหรับทุกความต้องการเกี่ยวกับวันที่และเวลา นี่คือวิธีการเพิ่มหรือลบวันที่:

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("เวลา UTC ปัจจุบัน: {}", now);

    let two_weeks = Duration::weeks(2);
    let future_date = now + two_weeks;
    println!("UTC ในสองสัปดาห์: {}", future_date);

    let thirty_days_ago = Duration::days(-30);
    let past_date = now + thirty_days_ago;
    println!("UTC 30 วันที่แล้ว: {}", past_date);
}
```

ตัวอย่างผลลัพธ์:

```
เวลา UTC ปัจจุบัน: 2023-04-01T12:00:00Z
UTC ในสองสัปดาห์: 2023-04-15T12:00:00Z
UTC 30 วันที่แล้ว: 2023-03-02T12:00:00Z
```

## ศึกษาเพิ่มเติม
โดยทั่วไปการจัดการวันที่และเวลาเป็นเรื่องที่ยาก ระบบและภาษาโปรแกรมต่างๆ จัดการกับมันในหลายวิธี Rust's standard library ให้การทำงานพื้นฐาน แต่ `chrono` crate เป็นตัวเลือกหลัก

มีทางเลือกอื่นหรือไม่? แน่นอนคุณสามารถคำนวณวันที่ด้วยตนเองได้โดยการแปลงทุกอย่างเป็นเวลาสแตมป์ จัดการกับตัวเลขและแปลงกลับ หรือคุณอาจใช้ไลบรารีเฉพาะเวลาในภาษาอื่นๆ — Python มี `datetime`, JavaScript มี `Date` เป็นต้น

`chrono` crate ใน Rust ให้คุณประเภทที่ตระหนักถึงเขตเวลาเช่น `DateTime`, และช่วงเวลาตามที่เห็นข้างต้น มันจัดการกับสิ่งที่ยุ่งยากเช่นปีอธิกสุรทินและการปรับเวลาฤดูร้อนให้คุณไม่ต้องทำ มันยังทำการแยกวันที่และรูปแบบการแสดงผลวันที่ ทำให้เป็นโซลูชันที่ครอบคลุม

## ดูเพิ่มเติม
- `chrono` crate: https://crates.io/crates/chrono
- เอกสารเวลาของ Rust: https://doc.rust-lang.org/std/time/index.html
- บทความเกี่ยวกับวันที่และเวลาในหนังสือ "ภาษาโปรแกรม Rust": https://doc.rust-lang.org/book/ch10-02-traits.html (ค้นหาส่วนที่เกี่ยวข้องกับ DateTime)
