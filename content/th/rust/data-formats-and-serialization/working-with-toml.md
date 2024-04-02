---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:05.809585-06:00
description: "TOML \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32 serialization\
  \ \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\
  \u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C \u0E21\u0E31\
  \u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\
  \u0E31\u0E49\u0E07\u0E04\u0E48\u0E32. \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML \u0E40\u0E19\u0E37\u0E48\u0E2D\
  \u0E07\u0E08\u0E32\u0E01\u0E04\u0E27\u0E32\u0E21\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\
  \u0E30\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E0B\u0E36\u0E48\
  \u0E07\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E41\u0E1B\u0E25\u0E07\u0E40\u0E1B\u0E47\
  \u0E19 hash\u2026"
lastmod: '2024-03-17T21:57:56.018099-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32 serialization \u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C \u0E21\u0E31\u0E01\u0E43\
  \u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\
  \u0E07\u0E04\u0E48\u0E32. \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\
  \u0E32\u0E01\u0E04\u0E27\u0E32\u0E21\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\u0E30\u0E04\
  \u0E27\u0E32\u0E21\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E0B\u0E36\u0E48\u0E07\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E41\u0E1B\u0E25\u0E07\u0E40\u0E1B\u0E47\u0E19 hash\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไร & ทำไม?
TOML เป็นภาษา serialization ข้อมูลที่อ่านได้โดยมนุษย์ มักใช้สำหรับการตั้งค่า. โปรแกรมเมอร์ใช้ TOML เนื่องจากความง่ายและความชัดเจน ซึ่งสามารถแปลงเป็น hash map ใน Rust ได้อย่างง่ายดาย

## วิธีการ:
```Rust
// 1. รวม 'toml' crate ใน Cargo.toml ของคุณ
// [dependencies]
// toml = "0.5"

// 2. Deserialize TOML เข้าสู่ struct ใน Rust
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("เซิร์ฟเวอร์กำลังทำงานบน {}:{}", host, port);
    // ผลลัพธ์: เซิร์ฟเวอร์กำลังทำงานบน "localhost":8080
}
```

## ลงลึก
TOML ซึ่งย่อมาจาก Tom's Obvious, Minimal Language ถูกสร้างโดย Tom Preston-Werner ในปี 2013 มุ่งเป้าหมายที่จะมีความสามารถอ่านได้ง่ายกว่า JSON หรือ YAML สำหรับไฟล์การตั้งค่า TOML มีจุดเน้นอยู่ที่ไวยากรณ์ที่ไม่ก่อให้เกิดความเข้าใจผิด การเรียบง่าย และการแมปไปยังชนิดข้อมูลได้อย่างง่ายดาย

ทางเลือกอื่นๆ สำหรับ TOML ประกอบด้วย JSON, YAML, และ XML แต่ TOML ชนะในสถานการณ์ที่การอ่านได้และการแก้ไขไฟล์โดยผู้ไม่ใช่โปรแกรมเมอร์มีความสำคัญ เมื่อทำงานกับ TOML ใน Rust, serde ให้พื้นฐานที่แข็งแกร่งสำหรับการ serialization และ deserialization โดยใช้ traits ในการแมป TOML ไปยัง struct ของ Rust ได้โดยไม่มีความยากลำบาก

ความท้าทายขณะทำงานกับ TOML คือความเข้มงวดเกี่ยวกับชนิดและโครงสร้าง โปรแกรมเมอร์ต้องกำหนดระบบชนิดข้อมูล Rust ที่มีโครงสร้างเป็นอย่างดีซึ่งสะท้อนถึงสคีมาของข้อมูล TOML เพื่อใช้ประโยชน์จาก TOML ใน Rust ได้อย่างมีประสิทธิภาพ

## ดูเพิ่มเติม
- [เอกสาร TOML](https://toml.io/en/)
- [Crate serde_toml](https://docs.rs/serde_toml/)
- [หนังสือภาษาโปรแกรมมิ่ง Rust](https://doc.rust-lang.org/stable/book/)
- [Repo GitHub TOML](https://github.com/toml-lang/toml)
