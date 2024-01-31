---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:47.083710-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с YAML означает работу с данными в формате "YAML Ain't Markup Language" — это стандарт сериализации данных, удобный для человека. Программисты используют его для файлов конфигурации, хранения данных или в любом месте, где им нужны легко читаемые и записываемые структурированные данные.

## Как это сделать:

Для разбора и генерации YAML в Rust мы используем крейт `serde_yaml`, который опирается на `serde` для сериализации/десериализации.

Сначала добавьте зависимости в ваш `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Теперь давайте сериализуем структуру Rust в YAML:

```rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    debug: bool,
    environment: String,
    port: u16,
}

fn main() -> serde_yaml::Result<()> {
    let config = Config {
        debug: true,
        environment: "development".to_string(),
        port: 8080,
    };

    // Сериализация в YAML
    let yaml_string = serde_yaml::to_string(&config)?;
    println!("{}", yaml_string);
    // Вывод:
    // ---
    // debug: true
    // environment: "development"
    // port: 8080

    Ok(())
}
```

Для десериализации YAML в структуру Rust:

```rust
fn main() -> serde_yaml::Result<()> {
    let yaml_string = r#"
    debug: true
    environment: "development"
    port: 8080
    "#;

    let config: Config = serde_yaml::from_str(&yaml_string)?;
    println!("{:?}", config);
    // Вывод:
    // Config { debug: true, environment: "development", port: 8080 }

    Ok(())
}
```

## Подробнее

YAML был создан в 2001 году как удобная альтернатива XML. В отличие от JSON, YAML поддерживает комментарии и менее "шумный", что делает его излюбленным для файлов конфигурации. `serde_yaml` в Rust использует `serde` для преобразования данных, обеспечивая высокую совместимость и гибкость. Хотя `serde_json` чаще используется из-за всемирной распространенности JSON в API, `serde_yaml` преуспевает для локальных файлов конфигурации и данных. Стоит отметить, что слишком сложные возможности YAML редко используются и иногда даже не рекомендуются из-за потенциальных проблем с анализом.

## Смотрите также

Для дополнительного чтения и более сложных случаев использования:

- Официальная документация Serde: https://serde.rs/
- Документация крейта Serde YAML: https://docs.rs/serde_yaml/latest/serde_yaml/
- Официальная спецификация YAML: https://yaml.org/spec/1.2/spec.html
- Книга "Язык программирования Rust": https://doc.rust-lang.org/book/
