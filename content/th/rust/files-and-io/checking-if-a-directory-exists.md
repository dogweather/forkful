---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:03.756332-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07 Rust\
  \ (`std`) \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\
  \u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E21\u0E14\
  \u0E39\u0E25 `std::path::Path` \u0E41\u0E25\u0E30 `std::fs` \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E46\u2026"
lastmod: '2024-03-17T21:57:56.009119-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E02\u0E2D\u0E07 Rust (`std`) \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\
  \u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E1C\
  \u0E48\u0E32\u0E19\u0E42\u0E21\u0E14\u0E39\u0E25 `std::path::Path` \u0E41\u0E25\u0E30\
  \ `std::fs` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\
  \u0E2D\u0E07 Rust."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
ไลบรารีมาตรฐานของ Rust (`std`) มีฟังก์ชันในการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ผ่านโมดูล `std::path::Path` และ `std::fs` นี่คือตัวอย่างง่ายๆ โดยใช้วิธีการมาตรฐานของ Rust:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("ไดเรกทอรีนี้มีอยู่");
    } else {
        println!("ไดเรกทอรีนี้ไม่มีอยู่");
    }
}
```

ตัวอย่างผลลัพธ์ โดยสมมติว่าไดเรกทอรีนี้มีอยู่:
```
ไดเรกทอรีนี้มีอยู่
```

สำหรับสถานการณ์ที่ซับซ้อนมากขึ้นหรือคุณลักษณะเพิ่มเติม (เช่น การดำเนินการระบบไฟล์แบบอะซิงโครนัส) คุณอาจพิจารณาใช้ไลบรารีภายนอก เช่น `tokio` พร้อมกับโมดูล `fs` แบบอะซิงโครนัส โดยเฉพาะอย่างยิ่งหากคุณทำงานภายใต้รันไทม์แบบอะซิงโครนัส นี่คือวิธีที่คุณสามารถทำแบบเดียวกันกับ `tokio`:

ก่อนอื่น เพิ่ม `tokio` ไปยังไฟล์ `Cargo.toml` ของคุณ:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

จากนั้น ใช้ `tokio::fs` เพื่อตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่แบบอะซิงโครนัส:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("ไดเรกทอรีนี้มีอยู่");
            } else {
                println!("เส้นทางมีอยู่แต่ไม่ใช่ไดเรกทอรี");
            }
        },
        Err(_) => println!("ไดเรกทอรีนี้ไม่มีอยู่"),
    }
}
```

ตัวอย่างผลลัพธ์ โดยสมมติว่าไดเรกทอรีไม่มีอยู่:
```
ไดเรกทอรีนี้ไม่มีอยู่
```

ตัวอย่างเหล่านี้เน้นวิธีที่ Rust และระบบของมันเสนอทั้งวิธีการแบบซิงโครนัสและอะซิงโครนัสในการตรวจสอบการมีอยู่ของไดเรกทอรี ตอบสนองต่อความต้องการในการพัฒนาซอฟต์แวร์ที่หลากหลาย
