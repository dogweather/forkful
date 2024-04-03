---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:36.486215-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: crate `chrono` \u0E02\u0E2D\
  \u0E07 Rust \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\
  \u0E2D\u0E31\u0E19\u0E14\u0E31\u0E1A\u0E41\u0E23\u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32 \u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E21\
  \u0E31\u0E19\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C `Cargo.toml`\
  \ \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
lastmod: '2024-03-17T21:57:56.006311-06:00'
model: gpt-4-0125-preview
summary: "crate `chrono` \u0E02\u0E2D\u0E07 Rust \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E2D\u0E31\u0E19\u0E14\u0E31\u0E1A\u0E41\u0E23\
  \u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\
  \u0E32 \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\
  \u0E43\u0E08\u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\
  \u0E44\u0E1F\u0E25\u0E4C `Cargo.toml` \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
crate `chrono` ของ Rust เป็นตัวเลือกอันดับแรกสำหรับการจัดการวันที่และเวลา ตรวจสอบให้แน่ใจว่ามันอยู่ในไฟล์ `Cargo.toml` ของคุณ:

```toml
[dependencies]
chrono = "0.4"
```

ตอนนี้, มาเริ่มต้นการจัดรูปแบบวันที่เป็นสตริงกัน

```rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

fn main() {
    let date: DateTime<Utc> = Utc::now(); // รับวันที่และเวลา UTC ปัจจุบัน
    let formatted_date = date.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", formatted_date); // พิมพ์: 2023-03-15 14:30:45
}
```

## ศึกษาลึกลงไป
ก่อนหน้า `chrono`, ไลบรารีมาตรฐานของ Rust มีฟังก์ชันการจัดการวันที่และเวลาเพียงไม่กี่อย่างและฟังก์ชันเหล่านั้นก็ค่อนข้างเบื้องต้น `chrono` ได้พัฒนาต่อยอดจากพื้นฐานนั้นเพื่อให้มีฟังก์ชันการทำงานที่ครอบคลุมยิ่งขึ้น อีกทางเลือกหนึ่งอาจเป็น crate `time` ใหม่ของ Rust ที่มีเป้าหมายเพื่อให้ API ที่ปลอดภัยและใช้งานง่ายยิ่งขึ้น

เมื่อคุณแปลงวันที่เป็นสตริง, คุณกำลังทำการ serialize – การเปลี่ยนข้อมูลให้กลายเป็นรูปแบบที่สามารถแบ่งปันหรือเก็บข้อมูลได้ รูปแบบที่คุณเลือก (`%Y-%m-%d %H:%M:%S` ในกรณีนี้) ขึ้นอยู่กับคุณ, และ `chrono` รองรับรูปแบบต่างๆมากมาย

ภายใน, วันที่มักจะถูกเก็บเป็นเวลาที่แสดงเป็นวินาทีจากจุดเริ่มต้น, เช่น Unix epoch (วันที่ 1 มกราคม 1970) เมื่อคุณจัดรูปแบบวันที่, คุณคำนวณรูปแบบที่เข้าใจได้จากมนุษย์จากจำนวนนี้, โดยพิจารณาถึงเขตเวลาและวินาทีอึดพิเศษ

## ดูเพิ่มเติม
- เอกสารของ crate `chrono`: https://docs.rs/chrono/
- เอกสารของ crate `time` ของ Rust: https://docs.rs/time/
- ไวยากรณ์สำหรับจัดรูปแบบวันที่: http://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
