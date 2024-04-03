---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:54.020526-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Rust, \u0E40\
  \u0E23\u0E32\u0E43\u0E0A\u0E49 `format!` macro."
lastmod: '2024-03-17T21:57:55.973794-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Rust, \u0E40\u0E23\u0E32\u0E43\u0E0A\u0E49 `format!` macro."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีการ:
ใน Rust, เราใช้ `format!` macro:

```Rust
fn main() {
    let name = "Ferris";
    let greeting = format!("สวัสดี, {}!", name);
    println!("{}", greeting); // แสดงผล "สวัสดี, Ferris!"
}
```
`format!` macro ทำงานคล้ายกับ `println!`, แต่จะคืนค่าสตริงที่จัดรูปแบบแล้ว แทนที่จะพิมพ์มันออกมา

## ดำดิ่งลึกลงไป
Rust เลือกใช้ macros เช่น `format!` สำหรับการแทรกสตริงแทนที่จะใช้ syntax ในภาษา ทำไม? Macros มีความสามารถและยืดหยุ่น—ขยายฟังก์ชันของภาษาโดยไม่ต้องมี syntax ที่ซับซ้อน

ในอดีต ภาษาอย่าง C ใช้ฟังก์ชั่นเช่น `sprintf` ซึ่งดูไม่สะดวกและมีโอกาสเกิดข้อผิดพลาดได้ง่าย Rust's `format!` macro นั้นปลอดภัยกว่า เพราะป้องกันข้อผิดพลาดทั่วไป

มีทางเลือกอื่น เช่น การใช้ `+` ในการต่อสตริง หรือ `format_args!` macro เพื่อหลีกเลี่ยงการจัดสรรหน่วยความจำในกอง แต่เมื่อพูดถึงความง่ายและความชัดเจน, `format!` ถือว่าเป็นที่สุด

หมายเหตุเรื่องประสิทธิภาพ: `format!` จะจัดสรรหน่วยความจำ สำหรับโค้ดที่ต้องการประสิทธิภาพสูงสุด, พิจารณาวิธีอื่น เช่น การเขียนลงใน buffer โดยตรง

## ดูเพิ่มเติม
- คู่มือ Rust ทางการเกี่ยวกับ `format!`: https://doc.rust-lang.org/std/macro.format.html
- `format!` เทียบกับ `println!`: https://doc.rust-lang.org/book/ch01-02-hello-world.html
- Rust โดยตัวอย่างเกี่ยวกับการจัดรูปแบบ: https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
