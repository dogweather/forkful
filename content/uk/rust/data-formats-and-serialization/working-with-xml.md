---
date: 2024-01-26 04:36:45.593773-07:00
description: "\u042F\u043A: \u0412 Rust \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\
  \u0435 \u043E\u0431\u0440\u043E\u0431\u043B\u044F\u0442\u0438 XML \u0437\u0430 \u0434\
  \u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A, \u0442\u0430\u043A\u0438\u0445 \u044F\u043A `xml-rs`. \u0412\
  \u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C, \u0434\u043E\u0434\u0430\
  \u0432\u0448\u0438 `xml-rs = \"0.8\"` \u0434\u043E \u0432\u0430\u0448\u043E\u0433\
  \u043E `Cargo.toml`. \u041E\u0441\u044C \u044F\u043A\u2026"
lastmod: '2024-03-13T22:44:48.977603-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Rust \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043E\u0431\
  \u0440\u043E\u0431\u043B\u044F\u0442\u0438 XML \u0437\u0430 \u0434\u043E\u043F\u043E\
  \u043C\u043E\u0433\u043E\u044E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  , \u0442\u0430\u043A\u0438\u0445 \u044F\u043A `xml-rs`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як:
В Rust ви можете обробляти XML за допомогою бібліотек, таких як `xml-rs`. Встановіть, додавши `xml-rs = "0.8"` до вашого `Cargo.toml`. Ось як розібрати простий XML:

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
                println!("Початок: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Текст: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Кінець: {}", name);
            }
            Err(e) => {
                println!("Помилка: {}", e);
            }
            _ => {}
        }
    }
}
```

Вивід:
```
Початок: book
Початок: title
Текст: Rust in Action
Кінець: title
Початок: author
Текст: Tim McNamara
Кінець: author
Початок: year
Текст: 2021
Кінець: year
Кінець: book
```
Цей код читає XML потоково, обробляючи початкові та кінцеві елементи, а також текстові дані, фіксуючи кожен крок.

## Поглиблений Розгляд:
XML є старожилом у технічному світі, створений для вебу в кінці 90-х. Його дизайн сприяє читабельності (як для машин, так і для людей) та обширним самоописним даним.

Альтернативи? Звісно, JSON є сучасним стандартом для веб-API, легшим та менш навантаженим. Тим часом, YAML завоював прихильників для конфігурацій із своєю чистою розкладкою. Але XML ще довго не піде нікуди — величезні інфраструктури побудовані на його основі.

Під капотом, розбір XML у Rust опирається на шаблони ітераторів, утримуючи низьке використання пам'яті та високу продуктивність. Ви знайдете бібліотеки, такі як `serde-xml-rs`, для більш serde-подібного досвіду — знахідка для тих, хто звик до обробки JSON.

## Дивіться Також:
Для більшого про Rust та XML: 
- `serde-xml-rs` для сумісності Rust із serde: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Офіційна документація Rust (адже завжди корисно освіжити знання): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
