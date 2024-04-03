---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:50.517255-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19 Rust \u0E04\u0E38\u0E13\u0E21\u0E35\u0E17\
  \u0E32\u0E07\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E2A\u0E2D\u0E07\u0E17\u0E32\u0E07\u0E2B\
  \u0E25\u0E31\u0E01:\u2026"
lastmod: '2024-03-17T21:57:55.970604-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E15\
  \u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19 Rust\
  \ \u0E04\u0E38\u0E13\u0E21\u0E35\u0E17\u0E32\u0E07\u0E40\u0E25\u0E37\u0E2D\u0E01\
  \u0E2A\u0E2D\u0E07\u0E17\u0E32\u0E07\u0E2B\u0E25\u0E31\u0E01."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
การทำให้สตริงเริ่มต้นด้วยตัวพิมพ์ใหญ่ใน Rust คุณมีทางเลือกสองทางหลัก: ใช้ฟังก์ชันความสามารถของไลบรารีมาตรฐานหรือใช้งานกับเครตลำดับที่สามสำหรับความต้องการที่ซับซ้อนหรือเฉพาะเจาะจงมากขึ้น นี่คือวิธีที่คุณสามารถทำได้ทั้งสองอย่าง

### ใช้งานไลบรารีมาตรฐานของ Rust
ไลบรารีมาตรฐานของ Rust ไม่มีวิธีการตรงไปตรงมาเพื่อให้สตริงเริ่มต้นด้วยตัวพิมพ์ใหญ่ แต่คุณสามารถทำได้โดยการจัดการตัวอักษรของสตริง

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // ผลลัพธ์: Hello
}
```

### ใช้งานเครต `heck`
สำหรับวิธีการที่ตรงไปตรงมามากขึ้น โดยเฉพาะเมื่อทำงานในบริบทการประมวลผลข้อความขนาดใหญ่ คุณอาจต้องการใช้งานไลบรารีของฝ่ายที่สาม เช่น `heck` เครต `heck` นำเสนอฟังก์ชันการแปลงอักษรต่างๆ รวมถึงวิธีการง่ายๆ ในการทำให้สตริงเริ่มต้นด้วยตัวพิมพ์ใหญ่

ก่อนอื่น เพิ่ม `heck` ลงใน `Cargo.toml` ของคุณ:

```toml
[dependencies]
heck = "0.4.0"
```

จากนั้น ใช้มันเพื่อทำให้สตริงเริ่มต้นด้วยตัวพิมพ์ใหญ่:

```rust
extern crate heck; // ไม่จำเป็นใน Rust 2018 edition หรือหลังจากนั้น
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // ผลลัพธ์: Hello World
}
```

หมายเหตุ: เมธอด `to_title_case` ที่ `heck` นำเสนอทำให้ทุกคำในสตริงเริ่มต้นด้วยตัวพิมพ์ใหญ่ ซึ่งอาจมากกว่าที่คุณต้องการหากคุณต้องการให้เพียงตัวอักษรแรกของสตริงเท่านั้นที่เป็นตัวพิมพ์ใหญ่ ปรับการใช้งานตามความต้องการเฉพาะของคุณ
