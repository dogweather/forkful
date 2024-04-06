---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:20.662825-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Standard Library \u0E02\u0E2D\
  \u0E07 Rust \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E08\u0E33\u0E01\
  \u0E31\u0E14\u0E41\u0E15\u0E48\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E40\u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\
  \u0E38\u0E1A\u0E31\u0E19, \u0E16\u0E36\u0E07\u0E41\u0E21\u0E49\u0E27\u0E48\u0E32\
  \u0E08\u0E30\u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E43\u0E2B\u0E49\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E1B\u0E0F\u0E34\u0E17\u0E34\u0E19\u0E42\u0E14\u0E22\
  \u0E15\u0E23\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\
  \u0E32\u0E23\u0E17\u0E33."
lastmod: '2024-03-17T21:57:56.005340-06:00'
model: gpt-4-0125-preview
summary: "Standard Library \u0E02\u0E2D\u0E07 Rust \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\
  \u0E17\u0E35\u0E48\u0E08\u0E33\u0E01\u0E31\u0E14\u0E41\u0E15\u0E48\u0E23\u0E27\u0E14\
  \u0E40\u0E23\u0E47\u0E27\u0E43\u0E19\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E40\u0E27\
  \u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19, \u0E16\u0E36\u0E07\
  \u0E41\u0E21\u0E49\u0E27\u0E48\u0E32\u0E08\u0E30\u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\
  \u0E43\u0E2B\u0E49\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\
  \u0E1A\u0E31\u0E19\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E1B\u0E0F\u0E34\
  \u0E17\u0E34\u0E19\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E33."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:


### การใช้ Standard Library ของ Rust
Standard Library ของ Rust มีวิธีที่จำกัดแต่รวดเร็วในการรับเวลาปัจจุบัน, ถึงแม้ว่าจะไม่ได้ให้วันที่ปัจจุบันในรูปแบบปฏิทินโดยตรง นี่คือวิธีการทำ:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("เวลาปัจจุบัน: {} วินาทีนับตั้งแต่ยุค Unix.", n.as_secs()),
        Err(_) => panic!("SystemTime ก่อนยุค Unix!"),
    }
}
```

ผลลัพธ์:
```
เวลาปัจจุบัน: 1615390665 วินาทีนับตั้งแต่ยุค Unix.
```

### การใช้ไลบรารี Chrono
สำหรับฟังก์ชันการทำงานกับวันที่และเวลาอย่างครอบคลุม, รวมถึงการรับข้อมูลวันที่ปัจจุบัน, คุณควรใช้ไลบรารี `chrono` ก่อนอื่น, เพิ่ม `chrono` ไปยัง `Cargo.toml` ของคุณ:

```toml
[dependencies]
chrono = "0.4"
```

จากนั้น, คุณสามารถใช้ `chrono` เพื่อรับวันที่ปัจจุบันได้:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("วันที่ปัจจุบัน: {}-{}-{}", now.year(), now.month(), now.day());
}
```

ผลลัพธ์:
```
วันที่ปัจจุบัน: 2023-4-20
```

ไลบรารี `chrono` ทำให้การทำงานกับวันที่และเวลาเป็นเรื่องง่าย, โดยนำเสนอฟังก์ชันการทำงานที่หลากหลายไม่เพียงแค่การเรียกข้อมูลวันที่ปัจจุบันเท่านั้น, รวมถึงการแยกวิเคราะห์, การจัดรูปแบบ, และการดำเนินการคำนวณกับวันที่และเวลา
