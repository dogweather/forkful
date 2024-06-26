---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:47.083710-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0438\
  \ \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u0438 YAML \u0432 Rust \u043C\
  \u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C \u043A\u0440\
  \u0435\u0439\u0442 `serde_yaml`, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043E\
  \u043F\u0438\u0440\u0430\u0435\u0442\u0441\u044F \u043D\u0430 `serde` \u0434\u043B\
  \u044F \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438\
  /\u0434\u0435\u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438\
  .\u2026"
lastmod: '2024-03-13T22:44:44.702841-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0438 \u0433\
  \u0435\u043D\u0435\u0440\u0430\u0446\u0438\u0438 YAML \u0432 Rust \u043C\u044B \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C \u043A\u0440\u0435\u0439\u0442\
  \ `serde_yaml`, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043E\u043F\u0438\u0440\
  \u0430\u0435\u0442\u0441\u044F \u043D\u0430 `serde` \u0434\u043B\u044F \u0441\u0435\
  \u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438/\u0434\u0435\u0441\u0435\
  \u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
