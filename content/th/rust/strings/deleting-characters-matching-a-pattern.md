---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:26.597417-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \ \u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E40\u0E2D\u0E32\u0E2D\u0E2D\u0E01\u0E02\u0E2D\u0E07\u0E25\u0E33\u0E14\u0E31\
  \u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E40\u0E09\u0E1E\u0E32\u0E30\
  . \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\
  \u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21, \u0E1E\u0E32\u0E23\u0E4C\u0E2A\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.971927-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \ \u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E40\u0E2D\u0E32\u0E2D\u0E2D\u0E01\u0E02\u0E2D\u0E07\u0E25\u0E33\u0E14\u0E31\
  \u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E40\u0E09\u0E1E\u0E32\u0E30\
  ."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:
ใน Rust, เราสามารถใช้เมธอด `replace` จากประเภท `String` หรือ regex สำหรับรูปแบบที่ซับซ้อนมากขึ้น. นี่คือวิธีการทำ:

```rust
fn main() {
    let phrase = "Hello, _world_! -- Programming in Rust --".to_string();
    // แทนที่ขีดล่างด้วยไม่มีอะไร
    let cleaned = phrase.replace("_", "");
    println!("{}", cleaned);

    // ใช้ regex สำหรับรูปแบบที่ซับซ้อนมากขึ้น (อย่าลืมเพิ่ม regex crate ไปยัง Cargo.toml)
    let regex = regex::Regex::new(r"--.*?--").unwrap();
    let s = regex.replace_all(&cleaned, "");
    println!("{}", s);
}

// ผลลัพธ์:
// Hello, world! -- Programming in Rust --
// Hello, world!
```

## ลงลึก
การลบอักขระที่ตรงกับรูปแบบไม่ได้จำกัดอยู่ที่ Rust เท่านั้น; เป็นการทำงานทั่วไปในหลายภาษาการเขียนโปรแกรม. ในอดีต, เครื่องมือเช่น `sed` ใน Unix ถูกใช้ในการแปลงข้อความได้อย่างมีประสิทธิภาพ, และตอนนี้ภาษาต่างๆ จัดหาฟังก์ชันในตัวสำหรับการจัดการสตริง.

ใน Rust, วิธีการมาตรฐานคือการใช้ `replace` สำหรับรูปแบบคงที่ง่ายๆ. สำหรับไวลด์การ์ด, การซ้ำ, หรือการลบที่มีเงื่อนไข, เราหันไปใช้ regex. แพ็กเกจ regex เป็นเครื่องมือที่ใช้กันอย่างแพร่หลายสำหรับเรื่องนี้, แต่จำไว้ว่า, การดำเนินการของ regex มีค่าใช้จ่ายสูงในแง่ของประสิทธิภาพ, ดังนั้นใช้มันอย่างมีวิจารณญาณ.

การรับประกันความปลอดภัยของ Rust ยังขยายไปถึงการประมวลผลข้อความ. ขณะที่ในภาษาบางภาษาการจัดการสตริงอาจเป็นแหล่งของช่องโหว่ด้านความปลอดภัย เช่น buffer overflows, การออกแบบของ Rust ช่วยป้องกันปัญหาดังกล่าว.

## ดูเพิ่มเติม
- คู่มือ `String` ของ Rust: https://doc.rust-lang.org/std/string/struct.String.html
- คู่มือแพ็กเกจ `regex`: https://docs.rs/regex/
- หนังสือ Rust Regex: https://rust-lang-nursery.github.io/regex/
