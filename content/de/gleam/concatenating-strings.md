---
title:                "Zeichenketten verknüpfen"
date:                  2024-01-20T17:34:38.967165-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
String-Konkatenation klebt Wörter oder Sätze zusammen. Programmierer nutzen das, um dynamische Texte zu erzeugen oder Daten zu formatieren.

## How to: (Anleitung:)
```gleam
fn main() {
  let greeting = "Hallo "
  let name = "Welt!"
  let message = string.concat([greeting, name])
  io.println(message) // "Hallo Welt!"
}
```

## Deep Dive (Tiefer Tauchgang)
String-Konkatenation ist essentiell und allgegenwärtig, startend in den frühen Tagen der Programmierung. Alternativ zur Konkatenation könnte man `string_builder`s verwenden, um performanter größere Textmengen zu verarbeiten. In Gleam wird String-Konkatenation typsicher gehandhabt dank des starken Typensystems, wodurch Fehler wie das versehentliche Zusammenfügen nicht-string Typen verhindert werden.

## See Also (Siehe Auch)
- Gleam's official documentation on strings: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)