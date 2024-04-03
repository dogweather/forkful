---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:12.820443-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E23\u0E2D\u0E1A\
  \u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E02\u0E2D\u0E07 Rust \u0E17\u0E35\
  \u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E15\u0E31\u0E27\
  \u0E21\u0E31\u0E19\u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A unit, integration, \u0E41\u0E25\u0E30\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\
  \u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E20\u0E32\u0E22\u0E19\u0E2D\
  \u0E01 \u0E40\u0E17\u0E2A\u0E16\u0E39\u0E01\u0E17\u0E33\u0E40\u0E04\u0E23\u0E37\u0E48\
  \u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E14\u0E49\u0E27\u0E22 `#[test]`,\u2026"
lastmod: '2024-03-17T21:57:55.997267-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E23\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\
  \u0E02\u0E2D\u0E07 Rust \u0E17\u0E35\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\
  \u0E01\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E21\u0E31\u0E19\u0E23\u0E2D\u0E07\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A unit, integration, \u0E41\
  \u0E25\u0E30\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E48\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E40\u0E17\u0E2A\u0E16\u0E39\u0E01\u0E17\
  \u0E33\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E14\u0E49\
  \u0E27\u0E22 `#[test]`, \u0E41\u0E25\u0E30\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u0E43\u0E14\u0E46 \u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E17\u0E33\u0E40\u0E04\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E14\u0E31\u0E07\u0E01\u0E25\
  \u0E48\u0E32\u0E27\u0E08\u0E30\u0E16\u0E39\u0E01\u0E04\u0E2D\u0E21\u0E44\u0E1E\u0E25\
  \u0E4C\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E17\u0E2A\n\n#."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## วิธีการ:
กรอบการทดสอบของ Rust ที่มาพร้อมกับตัวมันรองรับการทดสอบ unit, integration, และเอกสารโดยไม่ต้องใช้ไลบรารี่ภายนอก เทสถูกทำเครื่องหมายด้วย `#[test]`, และฟังก์ชันใดๆ ที่ถูกทำเครื่องหมายดังกล่าวจะถูกคอมไพล์เป็นเทส

### การเขียน Unit Test:
วาง unit tests ในโมดูลที่กำลังทดสอบโดยใช้ `tests` ย่อยโมดูลที่ทำเครื่องหมายด้วย `#[cfg(test)]` เพื่อให้แน่ใจว่ามันจะถูกคอมไพล์เมื่อกำลังทดสอบเท่านั้น

```rust
// lib.rs หรือ main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

การรันเทส:
```shell
$ cargo test
```

ผลลัพธ์:
```shell
   Compiling ชื่อแพ็คเกจของคุณ v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (หรือ src/main.rs)

running 1 test
test tests::it_adds_two ... ok

ผลการทดสอบ: ok. 1 ผ่าน; 0 ล้มเหลว; 0 ถูกละเลย; 0 ถูกวัด; 0 ถูกกรองออก
```

### การเขียน Integration Tests:
Integration tests อยู่ในไดเร็กทอรี่ tests ที่ชั้นบนสุดของโปรเจ็กต์ของคุณ ถัดจาก `src` ไฟล์ `.rs` แต่ละไฟล์ใน `tests` จะถูกคอมไพล์เป็น crate แยกต่างหาก

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### การทดสอบด้วยไลบรารี่ยอดนิยมจากบุคคลที่สาม:
สำหรับคุณสมบัติการทดสอบที่กว้างขึ้น, ไลบรารี่ `proptest` สามารถสร้างช่วงข้อมูลนำเข้าที่หลากหลายเพื่อทดสอบฟังก์ชัน

เพิ่ม `proptest` ในฐานะ dev dependency ใน `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

ใช้ `proptest` เพื่อรันการทดสอบเดียวกันกับข้อมูลนำเข้าที่ถูกสร้างขึ้นอัตโนมัติจำนวนมาก:

```rust
// ภายใน tests/integration_test.rs หรือโมดูลที่ #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

สิ่งนี้ตรวจสอบว่า `add` ไม่ panic สำหรับช่วงของข้อมูลนำเข้า `i32` ที่กว้าง
