---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:43.368519-07:00
description: "\u0423 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\
  \u043D\u0456 \u043D\u0430 Rust \u0440\u043E\u0431\u043E\u0442\u0430 \u0437 YAML\
  \ (YAML Ain't Markup Language) \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443\
  \ \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 \u0442\u0430 \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445 \u0443 \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0456 YAML, \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442\u0456 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ \u0434\u0430\u043D\u0438\u0445,\u2026"
lastmod: '2024-03-13T22:44:48.971196-06:00'
model: gpt-4-0125-preview
summary: "\u0423 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\
  \u043D\u0456 \u043D\u0430 Rust \u0440\u043E\u0431\u043E\u0442\u0430 \u0437 YAML\
  \ (YAML Ain't Markup Language) \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443\
  \ \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 \u0442\u0430 \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445 \u0443 \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0456 YAML, \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442\u0456 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ \u0434\u0430\u043D\u0438\u0445,\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
