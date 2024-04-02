---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:53.751325-06:00
description: "\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E14\u0E49\u0E27\u0E22 Rust, \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML (YAML Ain't Markup Language) \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E41\u0E25\
  \u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A YAML,\u2026"
lastmod: '2024-03-17T21:57:56.015275-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E14\u0E49\u0E27\u0E22 Rust, \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML (YAML Ain't Markup Language) \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E41\u0E25\
  \u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A YAML,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## อะไร & ทำไม?

ในการเขียนโปรแกรมด้วย Rust, การทำงานกับ YAML (YAML Ain't Markup Language) หมายถึงการแยกและสร้างข้อมูลในรูปแบบ YAML, มาตรฐานการอนุกรมข้อมูลที่ใช้งานง่ายสำหรับมนุษย์ โปรแกรมเมอร์ผสานการจัดการ YAML ใน Rust เพื่อกำหนดค่าแอปพลิเคชัน, จัดการการตั้งค่า, หรือทำงานกับโครงสร้างข้อมูลที่ซับซ้อนในรูปแบบที่ชัดเจนและอ่านง่าย, โดยได้รับประโยชน์จากความง่ายดายกว่า JSON หรือ XML สำหรับไฟล์การกำหนดค่าและการแลกเปลี่ยนข้อมูล

## วิธีการ:

Rust ไม่รองรับ YAML ในไลบรารีมาตรฐานของมัน, ดังนั้นเราจึงมักใช้ crates ของบุคคลที่สาม เช่น `serde` (สำหรับการทำซีเรียล และดิซีเรียลข้อมูล) ร่วมกับ `serde_yaml`.

ก่อนอื่น, เพิ่มความสัมพันธ์ใน `Cargo.toml` ของคุณ:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

ตอนนี้, เรามาดูวิธีการทำซีเรียลข้อมูล YAML เป็นโครงสร้างของ Rust และวิธีการทำซีเรียลโครงสร้างของ Rust กลับเป็นสตริง YAML.

### การทำซีเรียลข้อมูล YAML เป็นโครงสร้างของ Rust

กำหนดโครงสร้าง Rust ที่สะท้อนข้อมูลที่คุณคาดหวังใน YAML ใช้ Serde attributes สำหรับการปรับแต่งหากจำเป็น

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

ผลลัพธ์ที่ได้จากการรันโค้ด Rust ข้างต้นจะเป็น:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### การทำซีเรียลโครงสร้างของ Rust เป็น YAML

ตัวอย่างนี้ใช้ `Config` struct จากส่วนก่อนหน้าและทำซีเรียลกลับเป็นรูปแบบ YAML

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

ผลลัพธ์ที่คาดหวังจะเป็นสตริงที่จัดรูปแบบ YAML:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

ตัวอย่างเหล่านี้แสดงวิธีการผสานการทำซีเรียลข้อมูล YAML และการสร้างในแอปพลิเคชัน Rust ของคุณอย่างมีประสิทธิภาพ, โดยใช้ crates ยอดนิยม `serde` และ `serde_yaml`, รองรับโครงสร้างข้อมูลที่ซับซ้อนและให้การกำหนดค่าที่เรียบง่าย, สามารถอ่านได้โดยมนุษย์
