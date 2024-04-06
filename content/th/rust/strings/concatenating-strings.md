---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:48.922705-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Rust \u0E43\u0E2B\u0E49\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\
  \ \u0E21\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\u0E01\u0E31\u0E19\u0E40\u0E25\u0E22\
  ."
lastmod: '2024-04-05T21:54:01.512615-06:00'
model: gpt-4-0125-preview
summary: "Rust \u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2B\u0E25\
  \u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\
  \u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E31\u0E19 \u0E21\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\u0E01\
  \u0E31\u0E19\u0E40\u0E25\u0E22."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีการ:
Rust ให้วิธีการหลายวิธีในการเชื่อมต่อข้อความด้วยกัน มาเริ่มกันเลย

### การใช้ตัวดำเนินการ `+`
```Rust
let hello = "Hello".to_string();
let world = " world!";
let result = hello + world;
println!("{}", result); // ผลลัพธ์: Hello world!
```
ตัวดำเนินการ `+` นำ `" world!"` ต่อท้าย `"Hello"`, แต่ระวัง, `hello` ต้องเป็น `String`, ไม่ใช่ slice

### มาโคร `format!`
```Rust
let mood = "happy";
let message = format!("Have a {} day!", mood);
println!("{}", message); // ผลลัพธ์: Have a happy day!
```
`format!` คล้ายกับ `println!`, ผสมตัวแปรเข้ากับข้อความ สะดวกมากสำหรับเทมเพลต

### การเพิ่มข้อความเข้าไปในสตริง
```Rust
let mut tip = "Remember to".to_string();
tip.push_str(" breathe.");
println!("{}", tip); // ผลลัพธ์: Remember to breathe.
```
`push_str` เป็นการเพิ่ม slice ไปยัง `String`. เหมาะสำหรับการเพิ่มข้อความทีละน้อย

## มุมลึก
การต่อสตริงไม่ใช่ความคิดใหม่ มันมีมาตั้งแต่ยุคเริ่มต้นของการเขียนโปรแกรม; อย่างไรก็ตาม เราต้องการนำคำมาผสานกันเสมอ

ใน Rust, `String` เป็นชนิดสตริงที่เติบโตได้, เปลี่ยนแปลงได้, ถือครองข้อมูลแบบ UTF-8 มีทางเลือกอื่นอย่าง `&str`, slice ของสตริง, ซึ่งเป็นมุมมองเข้าไปใน `String`

แต่ละวิธีมีข้อแลกเปลี่ยน:

- ตัวดำเนินการ `+` เหมาะสำหรับการต่อกันหนึ่งหรือสองครั้ง แต่จะกินตัวดำเนินการด้านซ้าย (มันเข้าครอบครอง) ทุกการใช้ `+` ก็จะจัดสรรหน่วยความจำเพิ่มเติมซึ่งอาจสะสมไป

- `format!` ไม่ยึดครอบครองค่าใด ๆ ซึ่งดี แต่มันอาจจะช้าลงเนื่องจากความยืดหยุ่นและการจัดสรรใหม่ทุกครั้งที่เรียก นับเป็นมีดสวิสสำหรับการประกอบสตริง

- `push_str` มีประสิทธิภาพสำหรับการเพิ่มทีละน้อย มันไม่จัดสรรหน่วยความจำเว้นแต่ว่า `String` ต้องการพื้นที่เพิ่ม

การโฟกัสที่ความเป็นเจ้าของและการยืมใน Rust หมายความว่ามันจัดการกับสตริงได้แตกต่างกว่าภาษาอื่น ๆ เช่น Python หรือ JavaScript ความแตกต่างนี้รับประกันความปลอดภัยด้านหน่วยความจำ แต่ก็อาจมาพร้อมกับความยากในการเรียนรู้

## ดูเพิ่มเติม
สำหรับการศึกษาเพิ่มเติม:
- หนังสือ Rust เกี่ยวกับสตริง: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust โดยตัวอย่างเกี่ยวกับสตริง: https://doc.rust-lang.org/rust-by-example/std/str.html
- Rust std::string::String API Docs: https://doc.rust-lang.org/std/string/struct.String.html
