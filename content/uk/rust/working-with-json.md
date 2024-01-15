---
title:                "Робота з json"
html_title:           "Rust: Робота з json"
simple_title:         "Робота з json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Завдання, в якісно складнить життя програмістів, часто зв'язані з операціями з JSON даними. Мова програмування Rust зробила роботу з цим форматом даних простішою та безпечнішою завдяки своїм типам та інструментам.

## Як

Для початку, встановіть пакет serde_json перейшовши у командному рядку у папку з вашим проектом та виконавши наступну команду:

```Rust
cargo add serde_json
```

Тепер створіть структуру даних для вашого JSON об'єкту:

```Rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct Person {
    name: String,
    age: usize,
    profession: String,
}
```

Серіалізуйте об'єкт у JSON формат:

```Rust
let person = Person {
    name: String::from("John"),
    age: 30,
    profession: String::from("Developer"),
};

let json = serde_json::to_string(&person).unwrap();
println!("{}", json);
```

Результат цього коду буде виглядати наступним чином:

```json
{
    "name": "John",
    "age": 30,
    "profession": "Developer"
}
```

Для десеріалізації JSON даних, необхідно використати наступний код:

```Rust
let data: Person = serde_json::from_str(&json).unwrap();
println!("{:?}", data);
```

Ви отримаєте об'єкт типу Person з відповідними значеннями полів.

## Глибока занурення

У мові програмування Rust існує багато різних інструментів для роботи з JSON даними, таких як serde_json, serde_json::value та serde_json::error. Кожен з них має свої особливості та може бути корисним для різних сценаріїв роботи з даними.

Для отримання додаткової інформації та прикладів використання, рекомендуємо ознайомитися з [офіційною документацією](https://serde.rs/json.html) бібліотеки serde_json та [прикладами коду](https://github.com/serde-rs/json/tree/master/examples).

## Дивіться також

- [Офіційна документація по пакету serde_json](https://docs.rs/serde_json/)
- [Стаття про роботу з JSON даними у Rust](https://medium.com/swlh/working-with-json-in-rust-c814bafd5c01)
- [Спільнота Rust на Reddit](https://www.reddit.com/r/rust/)