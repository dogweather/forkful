---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:32.339290-07:00
description: "\u042F\u043A: \u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438\
  \ \u0437 JSON \u0443 Rust \u0430\u043A\u0442\u0438\u0432\u043D\u043E \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\
  \ \u043A\u0440\u0435\u0439\u0442 `serde` \u0440\u0430\u0437\u043E\u043C \u0437 `serde_json`\
  \ \u0434\u043B\u044F \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\
  \u0456\u0457 \u0442\u0430 \u0434\u0435\u0441\u0435\u0440\u0456\u0430\u043B\u0456\
  \u0437\u0430\u0446\u0456\u0457. \u0421\u043F\u043E\u0447\u0430\u0442\u043A\u0443\
  \ \u043F\u0435\u0440\u0435\u043A\u043E\u043D\u0430\u0439\u0442\u0435\u0441\u044F\
  , \u0449\u043E\u2026"
lastmod: '2024-03-13T22:44:48.972749-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 JSON \u0443\
  \ Rust \u0430\u043A\u0442\u0438\u0432\u043D\u043E \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u043A\u0440\u0435\
  \u0439\u0442 `serde` \u0440\u0430\u0437\u043E\u043C \u0437 `serde_json` \u0434\u043B\
  \u044F \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ \u0442\u0430 \u0434\u0435\u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\
  \u0446\u0456\u0457."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
