---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
JSON — це формат, що використовує текст для обміну даними. Програмісти обробляють JSON, щоб легше інтегруватися з веб-сервісами, зберігати конфігурації та обмінюватися даними.

## Як це зробити:

```Rust
use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct User {
    id: u64,
    name: String,
    email: String,
}

fn main() {
    let data = r#"
        {
            "id": 1,
            "name": "Олексій",
            "email": "oleksii@example.com"
        }"#;

    let user: User = serde_json::from_str(data).unwrap();
    println!("{:?}", user);

    let user_json = serde_json::to_string(&user).unwrap();
    println!("{}", user_json);
}
```
Output:
```
User { id: 1, name: "Олексій", email: "oleksii@example.com" }
{"id":1,"name":"Олексій","email":"oleksii@example.com"}
```

## Поглиблений розгляд:
JSON (JavaScript Object Notation) з'явився із JavaScript, але зараз використовується повсюдно. Альтернативи — XML, YAML, графові бази. В Rust для роботи з JSON найчастіше іспользуєтся бібліотека `serde_json`, яка працює з Serde — потужною фреймворк для серіалізації та десеріалізації Rust структур.

## Дивіться також:

- Serde офіційний вебсайт: [https://serde.rs/](https://serde.rs/)
- `serde_json` документація: [https://docs.serde.rs/serde_json/](https://docs.serde.rs/serde_json/)
- JSON специфікація: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Відмінності між JSON і YAML: [https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json](https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json)