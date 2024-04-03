---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:55.182822-07:00
description: "\u041A\u0430\u043A: \u0412 Rust \u0434\u043B\u044F \u0440\u0430\u0431\
  \u043E\u0442\u044B \u0441 JSON \u043E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\u0441\u044F \u043A\u0440\u0435\u0439\
  \u0442\u044B `serde` \u0438 `serde_json`. \u0412\u043E\u0442 \u043A\u0430\u043A\
  \ \u0438\u0445 \u043F\u0440\u0438\u043C\u0435\u043D\u044F\u0442\u044C: \u0421\u043D\
  \u0430\u0447\u0430\u043B\u0430 \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435\
  \ \u0437\u0430\u0432\u0438\u0441\u0438\u043C\u043E\u0441\u0442\u0438 \u0432 `Cargo.toml`."
lastmod: '2024-03-13T22:44:44.704451-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Rust \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\
  \ JSON \u043E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u044E\u0442\u0441\u044F \u043A\u0440\u0435\u0439\u0442\u044B `serde`\
  \ \u0438 `serde_json`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Как:
В Rust для работы с JSON обычно используются крейты `serde` и `serde_json`. Вот как их применять:

Сначала добавьте зависимости в `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

Затем импортируйте крейты и определите структуру для представления ваших данных:

```rust
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

#[derive(Serialize, Deserialize, Debug)]
struct User {
    id: u64,
    name: String,
    email: String,
}

fn main() {
    // Сериализация
    let user = User {
        id: 1,
        name: "Jane Doe".to_string(),
        email: "jane.doe@example.com".to_string(),
    };
    let j = serde_json::to_string(&user).unwrap();
    println!("{}", j); // {"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}

    // Десериализация
    let e: User = serde_json::from_str(&j).unwrap();
    println!("{:?}", e);  // User { id: 1, name: "Jane Doe", email: "jane.doe@example.com" }
}
```

## Глубже:
Экосистема библиотеки `serde` — это фактически стандартное решение Rust для сериализации с момента его выпуска в 2015 году. Она поддерживает много форматов, помимо JSON. В качестве альтернативы вы можете встретить `json-rust` или `simd-json`, которые предлагают разные компромиссы в производительности. Важная деталь реализации, которую следует понимать, заключается в том, что десериализация `serde` требует, чтобы структуры данных были известны во время компиляции, что не так в более динамичных языках, как JavaScript.

## Смотрите также:
- Официальная документация Serde предоставляет исчерпывающее руководство: [Документация Serde](https://serde.rs)
- Подробная информация о крейте `serde_json`: [Крейт serde_json](https://docs.rs/serde_json)
- Больше о самом JSON: [Введение в JSON](https://www.json.org/json-ru.html)
- Для асинхронного программирования с JSON часто используются `tokio` и `async-std` в связке с `serde_json`.
