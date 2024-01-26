---
title:                "Wyszukiwanie i zamiana tekstu"
date:                  2024-01-20T17:57:50.716872-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wyszukiwanie i zamiana tekstu to podstawowe narzędzia umożliwiające szybką modyfikację ciągów znaków. Programiści używają ich do refaktoryzacji kodu, poprawiania błędów i automatyzacji zadań.

## How to: (Jak to zrobić:)
```Gleam
import gleam/string

fn main() {
  let text = "Jabłka są zielone i jabłka są smaczne"
  let newText = string.replace(text, "zielone", "czerwone")
  newText |> io.print
}
```
Output:
```
Jabłka są czerwone i jabłka są smaczne
```

## Deep Dive (Dogłębna analiza)
Historically, search and replace functionality emerged from the need to efficiently edit text files, with early examples seen in text editors like `sed` from Unix. In Gleam, search and replace are more ergonomic thanks to built-in `string` module functions like `replace`. It takes the original string, the substring to find, and the replacement. Unlike simple text editors or command-line tools, Gleam allows complex pattern matching, making it powerful for structured text alterations within a type-safe environment. Alternatives in other languages include `gsub` in Ruby or `str_replace` in PHP. Gleam's approach is concise and integrates seamlessly with its strong typing system, reducing potential errors at compile time.

## See Also (Zobacz także)
- Gleam Documentation: [https://gleam.run/book](https://gleam.run/book)
