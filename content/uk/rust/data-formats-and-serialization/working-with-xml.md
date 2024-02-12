---
title:                "Робота з XML"
aliases:
- /uk/rust/working-with-xml/
date:                  2024-01-26T04:36:45.593773-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## Що та Чому?
XML, що розшифровується як eXtensible Markup Language (розширювальна мова розмітки), є ніби балакучим кузеном JSON. Ви стикнетеся з XML при роботі зі старими системами, корпоративним програмним забезпеченням або API, які оминули "вагон JSON". Він життєво важливий для обміну даними, де XML стоїть на своєму.

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
