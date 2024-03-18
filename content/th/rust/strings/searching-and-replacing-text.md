---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:41.219646-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\
  \u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E2B\u0E32\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E20\u0E32\u0E22\u0E43\u0E19\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E25\u0E31\u0E1A\u0E2D\u0E2D\
  \u0E01\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2D\u0E37\u0E48\u0E19 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\
  \u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E01\u0E49\
  \u0E44\u0E02\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E23\u0E35\u0E41\u0E1F\u0E01\u0E40\
  \u0E15\u0E2D\u0E23\u0E4C\u0E42\u0E04\u0E49\u0E14 \u0E2B\u0E23\u0E37\u0E2D\u2026"
lastmod: '2024-03-17T21:57:55.972882-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E04\u0E37\
  \u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E2B\u0E32\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E20\u0E32\u0E22\u0E43\u0E19\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E25\u0E31\u0E1A\u0E2D\u0E2D\
  \u0E01\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2D\u0E37\u0E48\u0E19 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\
  \u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E01\u0E49\
  \u0E44\u0E02\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E23\u0E35\u0E41\u0E1F\u0E01\u0E40\
  \u0E15\u0E2D\u0E23\u0E4C\u0E42\u0E04\u0E49\u0E14 \u0E2B\u0E23\u0E37\u0E2D\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การค้นหาและแทนที่ข้อความคือกระบวนการของการหาสตริงภายในสตริงและสลับออกเป็นสิ่งอื่น โปรแกรมเมอร์ทำเช่นนี้เพื่อแก้ไขข้อมูล รีแฟกเตอร์โค้ด หรือ อัตโนมัติการจัดการข้อความ

## วิธีทำ:

```Rust
fn main() {
    let text = "Hello there!";
    let updated_text = text.replace("there", "world");
    println!("{}", updated_text); // จะพิมพ์ "Hello world!"
}
```

ตัวอย่างผลลัพธ์:
```
Hello world!
```

## Deep Dive
การค้นหาและแทนที่ข้อความได้มีมาตั้งแต่เครื่องมือแก้ไขข้อความเริ่มมีขึ้น อุปกรณ์เช่น sed ใน Unix ทำให้การประมวลผลข้อความเป็นกลุ่มกลายเป็นปฏิบัติการทั่วไป

Rust นำเสนอวิธีการที่มีประสิทธิภาพและปลอดภัย วิธี `replace` จากไลบรารีมาตรฐานของประเภท `str` นั้นตรงไปตรงมาและตรวจสอบที่เวลาคอมไพล์

ทางเลือกสำหรับ `replace` รวมถึง regex สำหรับรูปแบบที่ซับซ้อน หรือการทำซ้ำตัวอักษรเพื่อปรับแต่งโลจิกการแทนที่

ภายใต้ฝาครอบ, `replace` ใน Rust สร้าง `String` ใหม่, ทำการทำซ้ำผ่านข้อความต้นฉบับ, ค้นหาคำที่ตรงกัน, และจากนั้นสร้างข้อความใหม่พร้อมการแทนที่ มันจัดการกับ Unicode ได้ดีซึ่งไม่ใช่เรื่องง่าย

## ดูเพิ่มเติม
- เอกสารของ Rust เกี่ยวกับ `replace`: https://doc.rust-lang.org/std/primitive.str.html#method.replace
- กล่องสำหรับรูปแบบที่ซับซ้อนยิ่งขึ้น: https://crates.io/crates/regex
- คู่มือของ Sed สำหรับการอ้างอิงทางประวัติศาสตร์: https://www.gnu.org/software/sed/manual/sed.html
