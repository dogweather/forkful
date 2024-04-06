---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:57.967012-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: TOML, \u0447\u0442\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0435\
  \u0442 Tom's Obvious, Minimal Language, \u0431\u044B\u043B \u0441\u043E\u0437\u0434\
  \u0430\u043D \u0422\u043E\u043C\u043E\u043C \u041F\u0440\u0435\u0441\u0442\u043E\
  \u043D-\u0412\u0435\u0440\u043D\u0435\u0440\u043E\u043C \u0432 2013 \u0433\u043E\
  \u0434\u0443. \u0415\u0433\u043E \u0446\u0435\u043B\u044C\u044E \u044F\u0432\u043B\
  \u044F\u0435\u0442\u0441\u044F \u043F\u043E\u0432\u044B\u0448\u0435\u043D\u0438\u0435\
  \u2026"
lastmod: '2024-04-05T21:53:45.259869-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u0447\u0442\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ Tom's Obvious, Minimal Language, \u0431\u044B\u043B \u0441\u043E\u0437\u0434\u0430\
  \u043D \u0422\u043E\u043C\u043E\u043C \u041F\u0440\u0435\u0441\u0442\u043E\u043D\
  -\u0412\u0435\u0440\u043D\u0435\u0440\u043E\u043C \u0432 2013 \u0433\u043E\u0434\
  \u0443."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

## Как это сделать:
```Rust
// 1. Добавьте крейт 'toml' в ваш Cargo.toml
// [зависимости]
// toml = "0.5"

// 2. Десериализация TOML в структуру в Rust
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
    
    println!("Сервер работает на {}:{}", host, port);
    // Вывод: Сервер работает на "localhost":8080
}
```

## Погружение в детали
TOML, что означает Tom's Obvious, Minimal Language, был создан Томом Престон-Вернером в 2013 году. Его целью является повышение читабельности по сравнению с JSON или YAML для файлов конфигурации. Дизайн TOML сосредоточен на однозначном синтаксисе, минимализме и простом сопоставлении с типами данных.

Альтернативы TOML включают JSON, YAML и XML, но TOML выигрывает в сценариях, где критически важны читаемость человеком и редактирование файла не программистами. При работе с TOML в Rust, serde предоставляет прочную основу для сериализации и десериализации, используя трейты для безупречного сопоставления TOML со структурами Rust.

Одна из проблем при работе с TOML заключается в его строгости к типам и структуре. Программист должен определить хорошо структурированную систему типов Rust, отражающую схему данных TOML, чтобы эффективно использовать TOML в Rust.

## См. также
- [Документация TOML](https://toml.io/en/)
- [Крейт serde_toml](https://docs.rs/serde_toml/)
- [Книга о языке программирования Rust](https://doc.rust-lang.org/stable/book/)
- [GitHub-репозиторий TOML](https://github.com/toml-lang/toml)
