---
title:                "Робота з JSON"
date:                  2024-02-03T19:24:32.339290-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Робота з JSON (JavaScript Object Notation) у Rust полягає у розборі даних JSON до структур даних Rust та серіалізації структур даних Rust назад у JSON. Програмісти роблять це для взаємодії з веб API, файлами конфігурацій, або будь-яким форматом обміну даними, де використовується JSON через його легкість та читабельність для людини.

## Як:

Для роботи з JSON у Rust активно використовується крейт `serde` разом з `serde_json` для серіалізації та десеріалізації. Спочатку переконайтеся, що вони включені до вашого `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Приклад 1: Десеріалізація JSON у Rust Struct

Визначте структуру Rust та використовуйте макроси похідної для `Deserialize` та `Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("ID користувача: {}", user.id);
    println!("Ім'я користувача: {}", user.name);
    println!("Email користувача: {}", user.email);
}
```

**Вивід:**

```
ID користувача: 1
Ім'я користувача: Jane Doe
Email користувача: jane.doe@example.com
```

### Приклад 2: Серіалізація Rust Struct у JSON

Використовуючи ту ж структуру `User`:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Вивід:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Ці приклади демонструють базовий потік десеріалізації JSON у структури Rust та серіалізації структур Rust назад у рядки JSON. Serde надає багатий набір інструментів для роботи з JSON, включаючи взаємодію з необов'язковими полями, складною вкладеністю та типами, які не підтримуються JSON безпосередньо.
