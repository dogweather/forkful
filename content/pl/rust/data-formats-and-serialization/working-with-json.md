---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:13.323209-07:00
description: "Jak to zrobi\u0107: Aby pracowa\u0107 z JSON w Rust, intensywnie u\u017C\
  ywa si\u0119 skrzynki `serde` wraz z `serde_json` do serializacji i deserializacji.\
  \ Najpierw upewnij\u2026"
lastmod: '2024-03-13T22:44:35.207777-06:00'
model: gpt-4-0125-preview
summary: "Aby pracowa\u0107 z JSON w Rust, intensywnie u\u017Cywa si\u0119 skrzynki\
  \ `serde` wraz z `serde_json` do serializacji i deserializacji."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
Aby pracować z JSON w Rust, intensywnie używa się skrzynki `serde` wraz z `serde_json` do serializacji i deserializacji. Najpierw upewnij się, że dołączysz te do swojego `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Przykład 1: Deserializacja JSON do struktury Rust
Zdefiniuj strukturę Rust i użyj makr pochodnych dla `Deserialize` i `Serialize`:

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

    println!("ID użytkownika: {}", user.id);
    println!("Nazwa użytkownika: {}", user.name);
    println!("Email użytkownika: {}", user.email);
}
```

**Wyjście:**

```
ID użytkownika: 1
Nazwa użytkownika: Jane Doe
Email użytkownika: jane.doe@example.com
```

### Przykład 2: Serializacja struktury Rust do JSON
Używając tej samej struktury `User`:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Wyjście:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Te przykłady demonstrują podstawowy przepływ deserializacji JSON do struktur Rust i serializacji struktur Rust z powrotem do łańcuchów JSON. Serde dostarcza bogaty zestaw narzędzi do pracy z JSON, w tym obsługę pól opcjonalnych, złożone zagnieżdżenia i typy nieobsługiwane bezpośrednio przez JSON.
