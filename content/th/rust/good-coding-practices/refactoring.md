---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:31.115994-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E21\u0E32 refactor \u0E42\
  \u0E04\u0E49\u0E14 Rust \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E21\u0E35\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E07\u0E48\u0E32\u0E22\
  \u0E41\u0E25\u0E30\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1A\u0E33\u0E23\u0E38\u0E07\
  \u0E23\u0E31\u0E01\u0E29\u0E32\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\
  \u0E49\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.003381-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32 refactor \u0E42\u0E04\u0E49\u0E14 Rust \u0E2A\u0E48\u0E27\u0E19\
  \u0E40\u0E25\u0E47\u0E01\u0E46 \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E21\
  \u0E35\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E40\u0E02\u0E49\u0E32\
  \u0E43\u0E08\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\u0E30\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E1A\u0E33\u0E23\u0E38\u0E07\u0E23\u0E31\u0E01\u0E29\u0E32\u0E44\u0E14\u0E49\
  \u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19 \u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\
  \u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E04\u0E33\u0E19\u0E27\u0E13\u0E1C\u0E25\u0E23\u0E27\u0E21\u0E02\
  \u0E2D\u0E07\u0E40\u0E27\u0E01\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E02\u0E2D\u0E07\u0E08\
  \u0E33\u0E19\u0E27\u0E19\u0E40\u0E15\u0E47\u0E21."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:
มา refactor โค้ด Rust ส่วนเล็กๆ เพื่อให้มีรูปแบบที่เข้าใจง่ายและสามารถบำรุงรักษาได้ง่ายขึ้น เริ่มต้นด้วยฟังก์ชันที่คำนวณผลรวมของเวกเตอร์ของจำนวนเต็ม:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

ผลลัพธ์:
```
The sum is 15
```

ตอนนี้, มา refactor ให้ใช้ Rust ที่เข้าถึงทางนิยมมากขึ้นโดยใช้ iterators และเมธอด `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

ไม่มีการเปลี่ยนแปลงในผลลัพธ์—ยังคงเป็น `15`—แต่เวอร์ชันที่ถูก refactor นี้เรียบง่ายขึ้นและใช้จุดแข็งของ Rust เช่น borrowing และเมธอดของ iterator

## การศึกษาลึก
Refactoring มีรากฐานมาจากชุมชนของ Smalltalk และได้รับความนิยมในโลกของ Java โดยหนังสือของ Martin Fowler ที่มีชื่อว่า "Refactoring: Improving the Design of Existing Code" หลักการของมันคือสิ่งทั่วไปและใช้ได้กับ Rust เช่นกัน ซึ่งความปลอดภัยและความพร้อมเพรียงเป็นสิ่งที่สำคัญที่สุด Rust สนับสนุนการเขียนโค้ดที่เข้มแข็งโดยการจับปัญหาในเวลา compile ดังนั้นในระหว่างการ refactor, คอมไพเลอร์ของ Rust ทำหน้าที่เป็นตาข่ายความปลอดภัย

ทางเลือกในการ refactor ด้วยตนเองรวมถึงการใช้เครื่องมืออัตโนมัติ เช่น 'rustfmt' สำหรับการจัดรูปแบบโค้ดและ 'clippy' สำหรับการ lint ซึ่งสามารถแนะนำวิธีการเขียนโค้ดที่เข้าถึงทางนิยมมากขึ้น อย่างไรก็ตาม, การ refactor อย่างลึกซึ้งมักต้องการความเข้าใจที่พิจารณาถึงออกแบบของโค้ด ซึ่งเครื่องมือเหล่านี้ไม่สามารถทำแบบอัตโนมัติได้อย่างเต็มที่

ใน Rust, การ refactor อาจมุ่งเน้นไปที่การปรับปรุงการใช้ประเภทข้อมูล, การใช้ชีวิตของตัวแปรอย่างมีประสิทธิภาพ, การลดการจัดสรรที่ไม่จำเป็น, หรือการใช้แบบแผนความพร้อมเพรียง เช่นการใช้ `Arc<Mutex<T>>` เมื่อจำเป็น มันยังเป็นสิ่งทั่วไปในการเปลี่ยนจาก `unwrap()` ไปสู่การจัดการข้อผิดพลาดที่แสดงออกมาได้ดีขึ้นด้วย `Result<T, E>`

## ดูเพิ่มเติม
เพื่อการศึกษาเพิ่มเติมเกี่ยวกับการ refactor ใน Rust:

- The Rust Book: https://doc.rust-lang.org/book/
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
- Clippy, เครื่องมือ lint ของ Rust: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" โดย Martin Fowler: https://martinfowler.com/books/refactoring.html
