---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з JSON (JavaScript Object Notation) полягає у маніпуляції даними у легковазі знятому форматі. Програмісти використовують його для обміну даними між сервером та клієнтом і для збереження конфігурацій.

## Як це робити:
```gleam
import gleam/json
import gleam/should

// Структура для прикладу JSON
type User =
  User(name: String, age: Int)

// Конвертація з JSON до типу User
pub fn user_from_json(json_string: String) -> Result(User, String) {
  json_string
  |> json.from_string
  |> result.map(json.to_map)
  |> result.then(
    fn(map) {
      map
      |> map.get("name")
      |> result.and_then(json.to_string)
      |> result.and_then(fn(name) {
          map.get("age")
          |> result.and_then(json.to_int)
          |> result.map(fn(age) {
              User(name, age)
          })
        })
    }
  )
}

// Конвертація з типу User до JSON
pub fn user_to_json(user: User) -> String {
  json.object([
    tuple("name", user.name |> json.string),
    tuple("age", user.age |> json.int)
  ])
  |> json.to_string
}

// Приклади
pub fn main() {
  assert Ok(User("Alice", 30)) = user_from_json("{\"name\":\"Alice\",\"age\":30}")
  assert "{\"name\":\"Alice\",\"age\":30}" = user_to_json(User("Alice", 30))
}
```
## Поглиблено:
JSON з'явився у 2001 році. JSON простий та зрозумілий, але не єдиний: є альтернативи, як XML чи YAML. У Gleam, ми використовуємо модулі, як `gleam/json`, для роботи з JSON. Вони дозволяють легко парсити JSON та конвертувати в типи даних Gleam.

## Дивіться також:
- Документація по Gleam JSON: https://hexdocs.pm/gleam_json/gleam_json/
- Детальніше про JSON: https://www.json.org/json-en.html
- Порівняння JSON із XML: https://www.w3schools.com/js/js_json_xml.asp
