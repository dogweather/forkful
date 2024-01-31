---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке робота з YAML і чому програмісти це роблять?
YAML - це формат даних, легкий для людського ока і машини. Програмісти використовують його для конфігурацій, зберігання даних та обміну між мовами програмування, завдяки його простоті та гнучкості.

## How to:
```Rust
// Додайте в Cargo.toml залежність
serde = "1.0"
serde_yaml = "0.8"
serde_derive = "1.0"

// Приклад структури для серіалізації/десеріалізації
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    version: String,
    debug: bool,
}

// Серіалізація конфігурації в YAML
fn serialize_config() -> Result<String, serde_yaml::Error> {
    let config = Config {
        version: "1.0.0".to_string(),
        debug: true,
    };
    serde_yaml::to_string(&config)
}

// Десеріалізація YAML в конфігурацію
fn deserialize_config(yaml: &str) -> Result<Config, serde_yaml::Error> {
    serde_yaml::from_str(yaml)
}

fn main() {
    // Серіалізація
    match serialize_config() {
        Ok(yaml) => println!("Serialized YAML:\n{}", yaml),
        Err(e) => println!("Serialization failed: {}", e),
    }

    // Якщо вам подобається YAML...
    let yaml_str = "
version: '2.0.0'
debug: false
";
    // Десеріалізація
    match deserialize_config(yaml_str) {
        Ok(config) => println!("Deserialized config: {:?}", config),
        Err(e) => println!("Deserialization failed: {}", e),
    }
}
```

## Deep Dive
YAML виник у 2001 році як більш читабельна альтернатива XML та JSON. В Rust робота з YAML зазвичай через бібліотеку `serde_yaml`, що використовує `serde` для (де)серіалізації. Хоча JSON залишається популярнішим для API, YAML частіше використовують для складних конфігурацій через більшу читабельність.

## See Also
- [The YAML Specification](https://yaml.org/spec/)
- [serde_yaml crate documentation](https://docs.rs/serde_yaml/0.8.17/serde_yaml/)
- [Serde guide](https://serde.rs/)
