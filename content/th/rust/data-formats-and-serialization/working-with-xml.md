---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:22.070790-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Rust, \u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \ XML \u0E44\u0E14\u0E49\u0E14\u0E49\u0E27\u0E22 crates \u0E40\u0E0A\u0E48\u0E19\
  \ `xml-rs` \u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E42\u0E14\u0E22\u0E01\u0E32\
  \u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21 `xml-rs = \"0.8\"` \u0E25\u0E07\u0E43\u0E19\
  \ `Cargo.toml` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\
  \u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C XML \u0E07\u0E48\
  \u0E32\u0E22\u0E46."
lastmod: '2024-03-17T21:57:56.019174-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Rust, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23 XML \u0E44\u0E14\u0E49\u0E14\u0E49\u0E27\u0E22\
  \ crates \u0E40\u0E0A\u0E48\u0E19 `xml-rs` \u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\
  \u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21 `xml-rs = \"\
  0.8\"` \u0E25\u0E07\u0E43\u0E19 `Cargo.toml` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\
  \u0E2B\u0E4C XML \u0E07\u0E48\u0E32\u0E22\u0E46."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
ใน Rust, คุณสามารถจัดการ XML ได้ด้วย crates เช่น `xml-rs` ติดตั้งโดยการเพิ่ม `xml-rs = "0.8"` ลงใน `Cargo.toml` นี่คือวิธีการวิเคราะห์ XML ง่ายๆ:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("เริ่ม: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("ข้อความ: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("สิ้นสุด: {}", name);
            }
            Err(e) => {
                println!("ข้อผิดพลาด: {}", e);
            }
            _ => {}
        }
    }
}
```

ผลลัพธ์:
```
เริ่ม: book
เริ่ม: title
ข้อความ: Rust in Action
สิ้นสุด: title
เริ่ม: author
ข้อความ: Tim McNamara
สิ้นสุด: author
เริ่ม: year
ข้อความ: 2021
สิ้นสุด: year
สิ้นสุด: book
```
โค้ดนี้อ่านข้อมูล XML แบบสตรีม, จัดการกับองค์ประกอบเริ่มต้นและสิ้นสุด รวมถึงข้อมูลข้อความ, บันทึกทุกขั้นตอน

## ลงลึก:
XML เป็นเทคโนโลยีรุ่นเก่า, ถูกสร้างขึ้นสำหรับเว็บในช่วงปลาย 90s การออกแบบของมันส่งเสริมให้มีการอ่านที่ง่าย (ทั้งสำหรับเครื่องและคน) และข้อมูลที่อธิบายตัวเองอย่างละเอียด 

มีทางเลือกอื่นหรือไม่? แน่นอน, JSON เป็นตัวเลือกยอดนิยมในยุคสมัยใหม่สำหรับ Web API, เบากว่าและมีรายละเอียดน้อยกว่า ในขณะเดียวกัน YAML ก็ได้รับความนิยมสำหรับการกำหนดค่าด้วยโครงสร้างที่เรียบง่าย แต่ XML ก็ยังไม่ได้หายไปในเร็วๆ นี้—มีโครงสร้างพื้นฐานขนาดใหญ่ที่สร้างขึ้นบน XML

ภายใต้ฝาครอบ, การวิเคราะห์ XML ของ Rust ใช้รูปแบบ iterator, ทำให้การใช้หน่วยความจำต่ำและประสิทธิภาพสูง คุณจะพบ crates เช่น `serde-xml-rs` สำหรับประสบการณ์ที่คล้ายกับ serde ซึ่งเป็นข้อดีสำหรับผู้ที่คุ้นเคยกับการจัดการ JSON

## ดูเพิ่มเติม:
สำหรับข้อมูลเพิ่มเติมเกี่ยวกับ Rust และ XML:
- `serde-xml-rs` สำหรับความเข้ากันได้ของ Rust's serde: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- เอกสารอย่างเป็นทางการของ Rust (เพราะการทบทวนไม่เคยทำให้เสียหาย): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
