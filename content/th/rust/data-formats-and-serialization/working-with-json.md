---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:54.314285-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON \u0E43\u0E19 Rust \u0E15\
  \u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49 `crate serde` \u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A `serde_json` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\
  \u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E44\u0E14\u0E49\u0E23\u0E27\u0E21\u0E40\u0E2B\
  \u0E25\u0E48\u0E32\u0E19\u0E35\u0E49\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.016242-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\
  \u0E1A JSON \u0E43\u0E19 Rust \u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49 `crate\
  \ serde` \u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E1A `serde_json` \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E41\u0E25\u0E30\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E43\u0E2B\
  \u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E44\u0E14\
  \u0E49\u0E23\u0E27\u0E21\u0E40\u0E2B\u0E25\u0E48\u0E32\u0E19\u0E35\u0E49\u0E43\u0E19\
  \u0E44\u0E1F\u0E25\u0E4C `Cargo.toml` \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:
เพื่อทำงานกับ JSON ใน Rust ต้องใช้ `crate serde` ร่วมกับ `serde_json` เพื่อการสร้างและการแปลงข้อมูล ให้แน่ใจว่าคุณได้รวมเหล่านี้ในไฟล์ `Cargo.toml` ของคุณ:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### ตัวอย่างที่ 1: การแปลงข้อมูล JSON เป็น Struct ของ Rust
กำหนดโครงสร้าง Struct ของ Rust และใช้ derive macros สำหรับ `Deserialize` และ `Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("รหัสผู้ใช้: {}", user.id);
    println!("ชื่อผู้ใช้: {}", user.name);
    println!("อีเมลผู้ใช้: {}", user.email);
}
```

**ผลลัพธ์:**

```
รหัสผู้ใช้: 1
ชื่อผู้ใช้: Jane Doe
อีเมลผู้ใช้: jane.doe@example.com
```

### ตัวอย่างที่ 2: การสร้าง Struct ของ Rust เป็นข้อมูล JSON
ใช้โครงสร้าง Struct `User` เดิม:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**ผลลัพธ์:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

ตัวอย่างเหล่านี้แสดงขั้นตอนพื้นฐานของการแปลงข้อมูล JSON เป็นโครงสร้างของ Rust และการสร้างโครงสร้างของ Rust กลับเป็นสตริง JSON Serde ให้เครื่องมือที่หลากหลายสำหรับการทำงานกับ JSON รวมถึงการจัดการกับฟิลด์ที่ไม่บังคับ การซ้อนโครงสร้างที่ซับซ้อน และประเภทข้อมูลที่ JSON ไม่รองรับโดยตรง
