---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON to format wymiany danych, który pozwala na komunikację między różnymi językami programowania. Programiści używają go, bo jest prosty, lekki i elastyczny w obsłudze.

## Jak to zrobić:
```gleam
import gleam/serde_json
import gleam/dynamic.{Dynamic, from_int, from_list, from_map, to_string}

pub fn main() -> Nil {
  let data = from_map([
    ("name", from_string("Jan")),
    ("age", from_int(30)),
    ("hobbies", from_list([from_string("Piłka nożna"), from_string("Programowanie")])),
  ])

  case serde_json.encode(data) {
    Ok(json) -> io.println(json)
    Error(_) -> io.println("Nie udało się zakodować danych do JSON.")
  }
}
```

Output:
```json
{"name":"Jan","age":30,"hobbies":["Piłka nożna","Programowanie"]}
```

## Zanurz się głębiej
JSON, czyli JavaScript Object Notation, powstał jako część języka JavaScript w latach 2000. Alternatywą dla JSON jest XML, ale JSON wygrywa ze względu na prostotę i mniejsze rozmiary plików. W Gleam, operacje na JSON wykonuje się za pomocą modułu `serde_json`, który dostarcza funkcji `encode` i `decode` do konwersji danych między typami `Dynamic` a JSON.

## Zobacz również
- JSON: [https://www.json.org/json-pl.html](https://www.json.org/json-pl.html)
- Gleam Book: [https://gleam.run/book](https://gleam.run/book)
