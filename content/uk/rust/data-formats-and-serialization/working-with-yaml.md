---
title:                "Робота з YAML"
date:                  2024-02-03T19:27:43.368519-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

У програмуванні на Rust робота з YAML (YAML Ain't Markup Language) полягає у парсингу та генерації даних у форматі YAML, стандарті серіалізації даних, зручний для людини. Програмісти інтегрують обробку YAML у Rust для налаштування додатків, управління налаштуваннями або обробки складних структур даних у чіткому та зрозумілому форматі, використовуючи його простоту порівняно з JSON або XML для файлів конфігурації та обміну даними.

## Як:

Rust не підтримує YAML у своїй стандартній бібліотеці, тому ми зазвичай використовуємо сторонні крейти, такі як `serde` (для серіалізації та десеріалізації даних) у комбінації з `serde_yaml`.

Спершу додайте залежності у ваш `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Тепер давайте подивимося, як десеріалізувати рядок YAML у структуру Rust та серіалізувати структуру Rust назад у рядок YAML.

### Десеріалізація YAML у структури Rust

Визначте структуру Rust, яка відображає дані, які ви очікуєте у YAML. Використовуйте атрибути Serde для налаштування за потреби.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

Приклад виводу при запуску вищевказаного коду Rust буде:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Серіалізація структур Rust у YAML

Цей приклад бере структуру `Config` з попереднього розділу та серіалізує її назад у формат YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

Очікуваний вивід буде у форматі YAML:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

Ці фрагменти демонструють, як ефективно інтегрувати парсинг та генерацію YAML у ваші додатки Rust, використовуючи популярні крейти `serde` та `serde_yaml`, адаптуючи складні структури даних та забезпечуючи прості, легкочитаємі конфігурації.
