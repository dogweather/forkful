---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:24.417589-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ HTML \u0E43\u0E19 Rust \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\u0E32\
  \u0E23 HTML \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\
  \u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E40\u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E32\u0E1B, \u0E01\u0E32\u0E23\
  \u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E04\
  \u0E23\u0E2D\u0E27\u0E4C\u0E40\u0E25\u0E2D\u0E23\u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.990047-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ HTML \u0E43\u0E19 Rust \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\u0E32\
  \u0E23 HTML \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\
  \u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E40\u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E32\u0E1B, \u0E01\u0E32\u0E23\
  \u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E04\
  \u0E23\u0E2D\u0E27\u0E4C\u0E40\u0E25\u0E2D\u0E23\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การแยกข้อมูล HTML ใน Rust คือการสกัดข้อมูลจากเอกสาร HTML ซึ่งเป็นสิ่งจำเป็นสำหรับการเว็บสกราป, การสกัดข้อมูล, หรือการสร้างเว็บครอว์เลอร์ โปรแกรมเมอร์ทำสิ่งนี้เพื่อเป็นการอัตโนมัติการรวบรวมข้อมูลจากเว็บ, การวิเคราะห์เนื้อหาเว็บ, หรือการย้ายเนื้อหาจากแพลตฟอร์มหนึ่งไปยังอีกแพลตฟอร์มหนึ่ง

## วิธีการ:

เพื่อแยกข้อมูล HTML ใน Rust, คุณมักจะใช้ `scraper` crate, ซึ่งให้บริการอินเทอร์เฟซระดับสูงในการเดินทางและจัดการเอกสาร HTML

ก่อนอื่น, เพิ่ม `scraper` เข้าไปใน `Cargo.toml` ของคุณ:

```toml
[dependencies]
scraper = "0.12.0"
```

ต่อไป, นี่คือตัวอย่างง่ายๆ ที่สกัด URL ของลิงก์ทั้งหมดจากสตริง HTML ที่กำหนด:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">ลิงก์ 1</a>
        <a href="http://example.com/2">ลิงก์ 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("พบลิงก์: {}", link);
    }
}
```

ผลลัพธ์:

```
พบลิงก์: http://example.com/1
พบลิงก์: http://example.com/2
```

ในตัวอย่างนี้, เราทำการแยกข้อมูลเอกสาร HTML ง่ายๆ เพื่อค้นหาองค์ประกอบ `<a>` ทั้งหมดและสกัดแอตทริบิวต์ `href` ของมัน, อย่างมีประสิทธิภาพทำให้สามารถพิมพ์ URL ของลิงก์ทั้งหมดในเอกสารได้ ห้องสมุด `scraper` ทำให้การแยกข้อมูล HTML และการเลือกองค์ประกอบเฉพาะโดยใช้ CSS เซเล็กเตอร์ง่ายขึ้น, ทำให้เป็นตัวเลือกที่ดีสำหรับงานเว็บสกราปใน Rust
