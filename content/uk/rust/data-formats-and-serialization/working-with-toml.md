---
title:                "Робота з TOML"
aliases:
- /uk/rust/working-with-toml.md
date:                  2024-01-26T04:26:38.004700-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## Що та Чому?
TOML - це мова серіалізації даних, призначена для людського читання, яку часто використовують для конфігурацій. Програмісти використовують TOML через його простоту та ясність, що легко перетворюється на хеш-мапу в Rust.

## Як це зробити:
```Rust
// 1. Додайте крейт 'toml' до вашого Cargo.toml
// [залежності]
// toml = "0.5"

// 2. Десеріалізуйте TOML у структуру в Rust
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
    
    println!("Сервер працює на {}:{}", host, port);
    // Вивід: Сервер працює на "localhost":8080
}
```

## Поглиблений Огляд
TOML, що означає "Tom's Obvious, Minimal Language" (Очевидна, Мінімалістична Мова Тома), була створена Томом Престон-Вернером у 2013 році. Мета полягає в тому, щоб бути більш читабельною, ніж JSON або YAML для файлів конфігурацій. Дизайн TOML зосереджений на недвозначному синтаксисі, мінімалізмі та легкому картографуванні до типів даних.

Альтернативи TOML включають JSON, YAML та XML, але TOML перемагає в сценаріях, де критично важливі читабельність людиною та редагування файлів не програмістами. Працюючи з TOML у Rust, serde надає міцну основу для серіалізації та десеріалізації, використовуючи трейти для беззусильного картографування TOML на структури Rust.

Виклик, працюючи з TOML, полягає в його строгості щодо типів і структури. Програміст повинен визначити добре структуровану систему типів Rust, що відображає схему даних TOML, для ефективного використання TOML у Rust.

## Дивіться Також
- [Документація TOML](https://toml.io/en/)
- [Крейт serde_toml](https://docs.rs/serde_toml/)
- [Книга Мова Програмування Rust](https://doc.rust-lang.org/stable/book/)
- [GitHub Репозиторій TOML](https://github.com/toml-lang/toml)
