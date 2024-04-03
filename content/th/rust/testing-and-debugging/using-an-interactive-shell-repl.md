---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:28.725863-06:00
description: "Shell \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A (REPL) \u0E02\u0E2D\u0E07\
  \ Rust, \u0E2B\u0E23\u0E37\u0E2D REPL (Read-Eval-Print Loop) \u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\
  \u0E19\u0E42\u0E04\u0E49\u0E14 Rust \u0E44\u0E14\u0E49\u0E17\u0E31\u0E19\u0E17\u0E35\
  \u0E41\u0E1A\u0E1A\u0E40\u0E2B\u0E47\u0E19\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\
  \u0E17\u0E31\u0E19\u0E17\u0E35\u2026"
lastmod: '2024-03-17T21:57:55.995223-06:00'
model: gpt-4-0125-preview
summary: "Shell \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A (REPL) \u0E02\u0E2D\u0E07 Rust,\
  \ \u0E2B\u0E23\u0E37\u0E2D REPL (Read-Eval-Print Loop) \u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E19\
  \u0E42\u0E04\u0E49\u0E14 Rust \u0E44\u0E14\u0E49\u0E17\u0E31\u0E19\u0E17\u0E35\u0E41\
  \u0E1A\u0E1A\u0E40\u0E2B\u0E47\u0E19\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E17\
  \u0E31\u0E19\u0E17\u0E35 \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\u0E49\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E21\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E2A\u0E48\u0E27\u0E19\u0E23\u0E2B\u0E31\u0E2A \u0E14\u0E35\u0E1A\u0E31\u0E01\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E41\u0E04\u0E48\u0E40\
  \u0E25\u0E48\u0E19\u0E01\u0E31\u0E1A\u0E1F\u0E35\u0E40\u0E08\u0E2D\u0E23\u0E4C\u0E02\
  \u0E2D\u0E07\u0E20\u0E32\u0E29\u0E32\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E21\u0E35\
  \u0E20\u0E32\u0E23\u0E30\u0E1C\u0E39\u0E01\u0E1E\u0E31\u0E19\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E04\u0E2D\u0E21\u0E44\u0E1E\u0E25\u0E4C\u0E42\u0E1B\u0E23\u0E40\u0E08\
  \u0E47\u0E04\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไรและทำไม?
Shell โต้ตอบ (REPL) ของ Rust, หรือ REPL (Read-Eval-Print Loop) ช่วยให้คุณสามารถรันโค้ด Rust ได้ทันทีแบบเห็นผลลัพธ์ทันที ซึ่งเหมาะสำหรับการทดลองหรือการเรียนรู้ โปรแกรมเมอร์ใช้มันเพื่อทดสอบส่วนรหัส ดีบัก หรือเพียงแค่เล่นกับฟีเจอร์ของภาษาโดยไม่มีภาระผูกพันของการคอมไพล์โปรเจ็คทั้งหมด

## วิธีการ:
ตอนนี้ Rust ยังไม่มี REPL ที่เป็นทางการมาพร้อมกับมัน คุณสามารถใช้เครื่องมือของบุคคลที่สามเช่น `evcxr_repl` ติดตั้งมันด้วย Cargo:

```sh
cargo install evcxr_repl
```

จากนั้น รัน REPL:

```sh
evcxr
```

ภายใน REPL ลองทดสอบโค้ด Rust ดู:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

ผลลัพธ์ควรเป็น:

```
5 + 3 = 8
```

## วิเคราะห์ลึก
จิตวิญญาณของ Rust มุ่งเน้นไปที่ความปลอดภัยและประสิทธิภาพ ซึ่งมักจะเชื่อมโยงกับภาษาที่คอมไพล์ล่วงหน้า และน้อยกว่ากับภาษาที่ตีความได้ มิตรกับ REPL ในอดีต ภาษาเช่น Python หรือ Ruby ให้ความสำคัญกับการมี REPL สำหรับการได้รับผลตอบกลับทันที แต่ไม่ได้ออกแบบมาเพื่องานระดับระบบ

แม้ว่าจะไม่มี REPL อย่างเป็นทางการใน Rust แต่ก็มีทางเลือกอื่นๆ เช่น `evcxr_repl` ปรากฏขึ้น โครงการเหล่านี้ไม่เพียงแหกกฎ Rust เข้าสู่ REPL เท่านั้น แต่ยังรวมกันอย่างชาญฉลาดระหว่างรอบของการคอมไพล์และรันเข้ากับเซสชั่นโต้ตอบ REPL คอมไพล์โค้ดลับๆ และรันไบนารี จับผลลัพธ์ออกมา ด้วยวิธีนี้ช่วยรักษาประโยชน์ด้านประสิทธิภาพของ Rust ไว้ พร้อมกับให้ประสบการณ์แบบโต้ตอบ

มีการอภิปรายกันอย่างต่อเนื่องในชุมชน Rust เกี่ยวกับการสนับสนุน REPL อย่างเป็นทางการ และกับการพัฒนาภาษาในแต่ละครั้ง เราเห็นความซับซ้อนของเครื่องมือเพิ่มมากขึ้น ซึ่งอาจจะนำไปสู่การมีโซลูชันแบบพื้นเมืองในที่สุด

## ดูข้อมูลเพิ่มเติม
สำหรับข้อมูลเพิ่มเติมและเครื่องมืออื่นๆ:
- ห้องสมุด Evcxr REPL GitHub: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, วิธีออนไลน์ในการทดสอบโค้ด Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- การอภิปรายของชุมชนภาษา Rust เกี่ยวกับฟีเจอร์ REPL: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
