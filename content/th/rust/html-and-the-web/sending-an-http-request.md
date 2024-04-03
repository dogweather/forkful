---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:57.349014-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E08\u0E32\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\
  \u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\
  \u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E2B\u0E23\u0E37\
  \u0E2D API \u2014 \u0E44\u0E21\u0E48\u0E27\u0E48\u0E32\u0E08\u0E30\u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.988092-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E08\u0E32\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\
  \u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\
  \u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E2B\u0E23\u0E37\
  \u0E2D API \u2014 \u0E44\u0E21\u0E48\u0E27\u0E48\u0E32\u0E08\u0E30\u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\
  \ \u0E42\u0E1E\u0E2A\u0E15\u0E4C\u0E2D\u0E31\u0E1B\u0E40\u0E14\u0E15, \u0E04\u0E38\
  \u0E13\u0E01\u0E47\u0E1A\u0E2D\u0E01\u0E21\u0E32\u0E40\u0E16\u0E2D\u0E30."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
ในการส่งคำขอ GET ใน Rust, เราใช้ ครีต `reqwest` ขั้นแรก, เพิ่มเข้าไปในไฟล์ `Cargo.toml` ของคุณ:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

ตอนนี้, เขียนโค้ด Rust แบบ async:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response_text = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    
    println!("คำตอบ: {}", response_text);
    Ok(())
}
```

ตัวอย่างผลลัพธ์อาจจะดูเหมือนนี้:

```
คำตอบ: {"key": "value", "hello": "world"}
```

นั่นคือสิ่งที่ต้องทำเพื่อส่งคำขอ GET ไปยังเอนด์พอยท์!

## ลงลึก
คำขอ HTTP เก่าแก่ประมาณหนึ่งในโลกอินเทอร์เน็ต เป็นแกนหลักของการสื่อสารบนเว็บ Rust ใช้ครีตเช่น `reqwest` เพราะมันไม่ใช่ภาษาเฉพาะเว็บ — ความยืดหยุ่นเป็นสิ่งสำคัญ `reqwest` ถูกสร้างขึ้นบน `hyper` ซึ่งเร็วและระดับต่ำ, แต่ `reqwest` เพิ่มความง่ายในการใช้งานเข้าไปอีก

มีทางเลือกอื่นที่ไม่ใช่ `reqwest` หรือ? แน่นอน `hyper` สำหรับผู้ที่ชื่นชอบความเร็ว, `surf` ถ้าคุณชอบ async Rust หรือ `ureq` สำหรับความง่าย — ไม่ต้องการความยุ่งยากกับ async

ลึกลงไปอีก, เมื่อคุณส่งคำขอ HTTP, Rust กำลังทำงานในสิ่งที่ภาษาใดๆ ก็ทำ: สร้างการเชื่อมต่อ TCP, ส่งคำขอ HTTP ที่จัดรูปแบบแล้ว, และตีความการตอบกลับแบบดิบ การจัดการคำขอแบบอะซิงโครนัสคือที่มาของความเก่งของ Rust, ช่วยให้คุณทำสิ่งอื่นๆ ขณะรอคำตอบจากเซิร์ฟเวอร์

## ดูเพิ่มเติม
- [เอกสารของ reqwest](https://docs.rs/reqwest/)
- [หนังสือ Rust Async](https://rust-lang.github.io/async-book/)
- [ไลบรารี HTTP Hyper](https://hyper.rs/)
- [คำแนะนำ API](https://rust-lang.github.io/api-guidelines/)
