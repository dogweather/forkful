---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:13.514579-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E22\u0E37\u0E19\u0E22\u0E31\u0E19\u0E15\u0E31\
  \u0E27\u0E15\u0E19\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2B\
  \u0E21\u0E32\u0E22\u0E04\u0E27\u0E32\u0E21\u0E27\u0E48\u0E32\u0E01\u0E32\u0E23\u0E43\
  \u0E2A\u0E48\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E41\u0E25\
  \u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u0E25\u0E07\u0E43\u0E19\u0E2A\
  \u0E48\u0E27\u0E19\u0E2B\u0E31\u0E27\u0E02\u0E2D\u0E07\u0E04\u0E33\u0E02\u0E2D\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E1E\u0E34\u0E2A\u0E39\u0E08\u0E19\u0E4C\u0E27\u0E48\u0E32\
  \u0E04\u0E38\u0E13\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E2D\u0E19\u0E38\u0E0D\u0E32\
  \u0E15\u0E43\u0E2B\u0E49\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u2026"
lastmod: '2024-03-17T21:57:55.992140-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E22\u0E37\u0E19\u0E22\u0E31\u0E19\u0E15\u0E31\
  \u0E27\u0E15\u0E19\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2B\
  \u0E21\u0E32\u0E22\u0E04\u0E27\u0E32\u0E21\u0E27\u0E48\u0E32\u0E01\u0E32\u0E23\u0E43\
  \u0E2A\u0E48\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E41\u0E25\
  \u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u0E25\u0E07\u0E43\u0E19\u0E2A\
  \u0E48\u0E27\u0E19\u0E2B\u0E31\u0E27\u0E02\u0E2D\u0E07\u0E04\u0E33\u0E02\u0E2D\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E1E\u0E34\u0E2A\u0E39\u0E08\u0E19\u0E4C\u0E27\u0E48\u0E32\
  \u0E04\u0E38\u0E13\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E2D\u0E19\u0E38\u0E0D\u0E32\
  \u0E15\u0E43\u0E2B\u0E49\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## อะไร & ทำไม?
การส่งคำขอ HTTP ด้วยการยืนยันตัวตนแบบพื้นฐานหมายความว่าการใส่ชื่อผู้ใช้และรหัสผ่านลงในส่วนหัวของคำขอเพื่อพิสูจน์ว่าคุณได้รับอนุญาตให้เข้าถึง ทำอย่างนี้เมื่อบริการต้องการให้แน่ใจว่ามันคือคุณ ไม่ใช่ Joe Schmoe บางคนที่พยายามเข้าถึงข้อมูล

## วิธีการ:

ก่อนอื่น ใส่ crate ที่จำเป็นลงใน `Cargo.toml` ของคุณ:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

ต่อไปนี้คือโค้ด Rust สำหรับการส่งคำขอ GET ด้วยการยืนยันตัวตนแบบพื้นฐาน:

```rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let user = "Aladdin";
    let password = "open sesame";
    
    let auth = Basic {
        username: user.into(),
        password: Some(password.into()),
    };
    
    let response = client
        .get("http://example.com/secrets")
        .header(Authorization(auth))
        .send()
        .await?;
    
    let content = response.text().await?;
    println!("Response: {}", content);
    
    Ok(())
}
```

ถ้าถูกต้อง มันจะพิมพ์ข้อมูลลับออกมา คุณเข้าใจแนวคิดนี้แล้ว

## ลงลึก

ก่อนหน้า `reqwest`, คุณจะเห็นคนจัดการกับ `curl` ใน Rust มันเหมือนกับการชอบใช้เลื่อยมือมากกว่าเลื่อยไฟฟ้า การยืนยันตัวตนแบบพื้นฐาน ถึงแม้จะง่ายดาย แต่ไม่ใช่ Fort Knox มันเพียงแค่การเข้ารหัส Base64 ของ "username:password" – ไม่มีการเข้ารหัส, ดังนั้น HTTPS จึงเป็นสิ่งจำเป็น

ทางเลือกอื่น? OAuth 2.0 เต้นรอบ ๆ Basic, โดยเสนอโทเค็นแทนข้อมูลรับรองที่จับต้องได้ อย่างไรก็ตาม มันซับซ้อน จากนั้นมีการยืนยันตัวตน Bearer, ถือโทเค็นเหมือนกับการจับมือลับ

โดยลึกซึ้ง, `reqwest` เป็นไคลเอ็นต์ HTTP ระดับสูงที่เข้ากันได้ดีกับคุณสมบัติ async ของ Rust 'Basic' struct สร้างส่วนหัว, 'Authorization' ใส่มันลงไป, และพร้อม, คุณกำลังเคาะประตูเซิร์ฟเวอร์ด้วยการกระซิบลับ

## ดูเพิ่มเติม

สำหรับเรื่องราวและเวทย์มนตร์เพิ่มเติม:

- เอกสาร reqwest: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- ทำความเข้าใจการยืนยัน Access พื้นฐานของ HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- การเขียนโปรแกรมแบบ async ใน Rust: [https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)
- เอกสาร crate base64 ของ rust: [https://docs.rs/base64](https://docs.rs/base64)
