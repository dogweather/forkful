---
title:                "Rust: Робота з json"
simple_title:         "Робота з json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

JSON (JavaScript Object Notation) є одним з найпопулярніших форматів обміну даними у світі програмування. Використання JSON дозволяє легко передавати та отримувати дані між програмами та платформами. У цій статті ми розглянемо, як працювати з JSON у мові програмування Rust.

## Як

Для роботи з JSON у Rust існує спеціальна бібліотека - serde_json. Для початку, встановіть її за допомогою менеджера пакетів Cargo:

```Rust
[dependencies]
serde_json = "1.0"
```

Тепер додайте імпорт бібліотеки у своїй програмі:

```Rust
extern crate serde_json;
use serde_json::{Value, Result};
```

Отримати дані з JSON можна за допомогою функції `from_str()` і використовуючи структуру `Value`. Наприклад, якщо ви маєте такий JSON:

```Rust
{
    "name": "John",
    "age": 30,
    "isMarried": false,
    "hobbies": ["reading", "hiking"]
}
```

Ви можете здійснити такий код для отримання інформації з нього:

```Rust
let json = r#"
{
    "name": "John",
    "age": 30,
    "isMarried": false,
    "hobbies": ["reading", "hiking"]
}
"#;

let data: Value = serde_json::from_str(json)?;

println!("Name: {}", data["name"]);
println!("Age: {}", data["age"]);
println!("Marital status: {}", data["isMarried"]);
println!("Hobbies: {}", data["hobbies"][0]);
```

Це виведе наступну інформацію:

```
Name: John
Age: 30
Marital status: false
Hobbies: reading
```

Також, можна використовувати функцію `to_string()` для перетворення даних у JSON строку. Наприклад, якщо ми маємо об'єкт `data` з попереднього прикладу, ми можемо отримати JSON строку таким чином:

```Rust
let json_string = serde_json::to_string(&data)?;
println!("{}", json_string);
```

Що виведе такий результат:

```
{"name":"John","age":30,"isMarried":false,"hobbies":["reading","hiking"]}
```

Це дуже корисно при роботі з API, які працюють з JSON даними.

## Глибоке вивчення

У більш складних випадках, коли потрібно серіалізувати чи десеріалізувати складну структуру даних, можна використовувати анотації даних (data annotation) для більш точного управління процесом серіалізації та десеріалізації. Також, бібліотека serde_json має багато додаткових функцій та можливостей, які дозволяють управляти процесом роботи з JSON ще більш ефективно.

## Дивіться також

- [Офіційна документація по бібліотеці serde_json](https://