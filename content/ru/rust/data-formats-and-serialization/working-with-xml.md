---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:55.208736-07:00
description: "\u041A\u0430\u043A: \u0412 Rust \u0432\u044B \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C \u0441 XML \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0442\u0430\u043A\u0438\u0445 \u043A\
  \u0440\u0435\u0439\u0442\u043E\u0432, \u043A\u0430\u043A `xml-rs`. \u0423\u0441\u0442\
  \u0430\u043D\u043E\u0432\u0438\u0442\u0435, \u0434\u043E\u0431\u0430\u0432\u0438\
  \u0432 `xml-rs = \"0.8\"` \u0432 \u0432\u0430\u0448 `Cargo.toml`. \u0412\u043E\u0442\
  \ \u043A\u0430\u043A \u0440\u0430\u0437\u043E\u0431\u0440\u0430\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.709365-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Rust \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0440\u0430\
  \u0431\u043E\u0442\u0430\u0442\u044C \u0441 XML \u0441 \u043F\u043E\u043C\u043E\u0449\
  \u044C\u044E \u0442\u0430\u043A\u0438\u0445 \u043A\u0440\u0435\u0439\u0442\u043E\
  \u0432, \u043A\u0430\u043A `xml-rs`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как:
В Rust вы можете работать с XML с помощью таких крейтов, как `xml-rs`. Установите, добавив `xml-rs = "0.8"` в ваш `Cargo.toml`. Вот как разобрать простой XML:

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
                println!("Начало: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Текст: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Конец: {}", name);
            }
            Err(e) => {
                println!("Ошибка: {}", e);
            }
            _ => {}
        }
    }
}
```

Вывод:
```
Начало: book
Начало: title
Текст: Rust in Action
Конец: title
Начало: author
Текст: Tim McNamara
Конец: author
Начало: year
Текст: 2021
Конец: year
Конец: book
```
Этот код считывает XML потоковым способом, обрабатывая начальные и конечные элементы, а также текстовые данные, регистрируя каждый шаг.

## Глубокое Погружение:
XML - это старожил в технологических годах, созданный для веба в конце 90-х. Его дизайн способствует читабельности (как для машин, так и для людей) и широкой самоописываемости данных.

Альтернативы? Конечно, JSON - это современный стандарт для веб-API, более легкий и менее загруженный. Тем временем YAML завоевал поклонников для конфигураций с его чистым оформлением. Но XML не собирается никуда уходить в ближайшее время - огромные инфраструктуры построены на его основе.

Под капотом, разбор XML в Rust опирается на шаблоны итераторов, сохраняя низкое использование памяти и высокую производительность. Вы найдете крейты вроде `serde-xml-rs` для более serde-подобного опыта - благо для тех, кто привык к обработке JSON.

## Смотрите Также:
Для дополнительной информации о Rust и XML:
- `serde-xml-rs` для совместимости с Rust serde: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Официальная документация Rust (ведь повторение - мать учения): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
