---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:32.339290-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0443 Rust \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0440\
  \u043E\u0437\u0431\u043E\u0440\u0456 \u0434\u0430\u043D\u0438\u0445 JSON \u0434\u043E\
  \ \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\u0430\u043D\u0438\u0445\
  \ Rust \u0442\u0430 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\
  \u0456\u0457 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\u0430\u043D\
  \u0438\u0445 Rust \u043D\u0430\u0437\u0430\u0434 \u0443 JSON.\u2026"
lastmod: '2024-03-13T22:44:48.972749-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0443 Rust \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0440\u043E\u0437\
  \u0431\u043E\u0440\u0456 \u0434\u0430\u043D\u0438\u0445 JSON \u0434\u043E \u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\u0430\u043D\u0438\u0445 Rust \u0442\
  \u0430 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\u0430\u043D\u0438\u0445\
  \ Rust \u043D\u0430\u0437\u0430\u0434 \u0443 JSON."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
