---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:20.236907-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с JSON (JavaScript Object Notation) означает обработку данных в повсеместно используемом текстовом формате, который легко понять как людям, так и машинам. Программисты делают это потому, что JSON является королем для хранения и передачи структурированных данных, особенно в веб-приложениях.

## Как это сделать:

Вот как обрабатывать JSON в Gleam, кодируя и декодируя данные. Вам понадобится пакет `gleam/json`, так что сначала его установите.

```gleam
import gleam/json

// Определите тип
pub type Person {
  Person(name: String, age: Int)
}

// Кодирование в JSON
pub fn encode_person(person: Person) -> json.Json {
  case person {
    Person(name, age) -> 
      json.object([
        "name", json.string(name),
        "age", json.int(age)
      ])
  }
}
// Пример использования и вывода
let john = Person("John Doe", 30)
let json_john = encode_person(john)
json_john // {"name": "John Doe", "age": 30}

// Декодирование из JSON
pub fn decode_person(json: json.Json) -> Result(Person, Nil) {
  let Ok(json) = json.decode_pair() // Декодируйте объект JSON
  let Ok(name) = json.field("name").map(json.decode_string)
  let Ok(age) = json.field("age").map(json.decode_int)
  person.Person(name, age)
}
// Пример использования и вывода
let decoded_person = decode_person(json_object("{\"name\": \"John Doe\", \"age\": 30}"))
decoded_person // Ok(Person("John Doe", 30))
```

## Подробнее

JSON существует с начала 2000-х годов, заменяя XML во многих сценариях за его простоту. Альтернативы включают YAML, XML и BSON, среди прочего, но простота использования JSON сохраняет его на переднем крае. В Gleam обработка JSON опирается на сопоставление с образцом и мощные функции библиотеки `gleam/json` для функционального подхода к кодированию и декодированию структур данных.

## Смотрите также

- Официальная документация по JSON в Gleam: [https://hexdocs.pm/gleam_json](https://hexdocs.pm/gleam_json)
- Введение в JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Руководство Mozilla Developer Network по JSON: [https://developer.mozilla.org/ru/docs/Learn/JavaScript/Objects/JSON](https://developer.mozilla.org/ru/docs/Learn/JavaScript/Objects/JSON)
