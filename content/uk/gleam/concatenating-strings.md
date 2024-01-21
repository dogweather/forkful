---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:34:45.334316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Concatenating strings is about sticking two or more pieces of text together. Programmers do it to create messages, build up file paths, or just because they need one string out of several parts.

## How to: (Як це зробити:)
```gleam
import gleam/string

fn main() {
  let greeting = "Hello, "
  let name = "Viktor"
  let exclamation = "!"

  // Using the `+` operator
  let message = greeting + name + exclamation
  string.print(message) // Output: Hello, Viktor!
}
```

Alternative with string interpolation:
```gleam
fn main() {
  let name = "Viktor"
  
  // Using string interpolation
  let message = "Hello, $name!"
  string.print(message) // Output: Hello, Viktor!
}
```

## Deep Dive (Глибоке занурення)
Concatenation has been around since early programming; it's not unique to Gleam. Alternatives include string interpolation (like in the second example) and specialized functions or methods in various languages. In Gleam, using the `+` operator is straightforward, but interpolation can be cleaner when inserting variables. Performance-wise, concatenating immutable strings can sometimes lead to inefficiencies due to the creation of multiple intermediate strings, but this rarely impacts most real-world applications in modern systems.

## See Also (Додатково)
- A Gleam forum discussion on string handling: [https://github.com/gleam-lang/gleam/discussions](https://github.com/gleam-lang/gleam/discussions)
- An article on efficient string handling in functional languages: search for "Functional programming and string manipulation" for insights applicable here too.