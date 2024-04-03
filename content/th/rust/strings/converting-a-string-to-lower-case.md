---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:34.480742-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:55.975138-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
```Rust
fn main() {
    let greeting = "HeLLo, WoRlD!";
    let lowercase_greeting = greeting.to_lowercase();
    println!("{}", lowercase_greeting); // "hello, world!"
}
```
ผลลัพธ์:
```
hello, world!
```

## ลงลึก
ก่อนจะมีเมธอด `.to_lowercase()` คุณอาจเคยเห็นคนที่ใช้ Rust ใช้งาน `.to_ascii_lowercase()` สำหรับงานเดียวกัน ซึ่งมีผลกระทบเฉพาะกับอักขระ ASCII เท่านั้น ไลบรารีมาตรฐานของ Rust ได้พัฒนาขึ้น โดยให้บริการ `.to_lowercase()` สำหรับการสนับสนุน Unicode แบบเต็มรูปแบบ—หมายความว่ามันสามารถจัดการได้มากกว่าแค่ภาษาอังกฤษ! นี่เป็นเรื่องที่สำคัญมากหากแอปพลิเคชันของคุณมีการใช้งานในโลกที่หลากหลายภาษา

มันทำงานอย่างไรใต้ฮู้ด? ดี, เมธอด `to_lowercase()` ไม่ได้เพียงแค่เปลี่ยน 'A' เป็น 'a' เท่านั้น มันเหมือนกับนักภาษาศาสตร์เล็ก ๆ ที่มีความรู้ด้าน Unicode เป็นอย่างดี มันปฏิบัติตามมาตรฐาน Unicode เพื่อแปลงอักขระให้เป็นตัวพิมพ์เล็กอย่างถูกต้องโดยคำนึงถึงลักษณะทางวัฒนธรรม

แน่นอน มีทางเลือกอื่น คุณสามารถสร้างลูป, ไล่ตัวอักษรแต่ละตัว และแปลงมันด้วยตัวเอง แต่ทำไมต้องคิดค้นเครื่องมือใหม่เมื่อไลบรารีมาตรฐานของ Rust ได้ทำงานนั้นไว้ให้แล้วล่ะ?

## ดูเพิ่มเติม
- [เอกสาร Rust เกี่ยวกับ `to_lowercase()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [เอกสาร String ของ Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [การแมปกรณี Unicode](https://www.unicode.org/reports/tr21/tr21-5.html)
