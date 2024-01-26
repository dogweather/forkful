---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:42:09.284162-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Usuwanie znaków pasujących do wzorca to operacja filtrująca ciąg tekstowy. Programiści robią to, by oczyścić dane, usunąć niepotrzebne fragmenty lub przygotować tekst do dalszej obróbki.

## How to: (Jak to zrobić:)
```Gleam
import gleam/regex

pub fn main() {
  let pattern = regex.from_string("[aeiou]")         // Określamy wzorzec - samogłoski
  assert Ok(regex) = pattern
  let result = regex.replace(regex, "Hello, World!", "") // Usuwamy pasujące znaki (samogłoski)
  
  io.debug(result)  // Wyświetlamy wynik: "Hll, Wrld!"
}
```

Sample output:
```
"Hll, Wrld!"
```

## Deep Dive (Dogłębna analiza):
Deleting characters based on patterns dates back to early command-line tools like `sed` and `grep` in Unix. In Gleam, we use the `gleam/regex` module. It provides pattern matching functionality, similar to other regex libraries in different programming languages.

Alternatives include using string functions like `string.replace` for simple replacements, but they lack the flexibility of regex patterns.

The `regex` module in Gleam uses Rust's `regex` crate under the hood, ensuring high performance and reliability.

## See Also (Zobacz także):
- Regular Expressions Basics: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Rust Regex crate: [https://docs.rs/regex/](https://docs.rs/regex/)
