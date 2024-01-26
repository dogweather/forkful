---
title:                "Zeichenketten interpolieren"
date:                  2024-01-20T17:50:49.917641-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ist das Einbetten von Variablenwerten direkt in String-Literale, wodurch dynamisch zusammengesetzte Nachrichten oder Daten möglich werden. Programmierer nutzen dies, um flexiblere und wartbare Codebases zu erstellen, indem sie Text mit variablen Daten einfach mischen.

## So geht's:
```gleam
fn main() {
  let name = "Welt"
  let greeting = "Hallo, \(name)!"
  io.println(greeting)
}
```
Ausgabe:
```
Hallo, Welt!
```

## Deep Dive
Die Interpolation von Strings wird schon seit Langem in vielen Programmiersprachen verwendet, um die String-Erstellung zu vereinfachen. Im Vergleich zu alternativen Methoden wie der String-Konkatenation (`"Hallo, " ++ name ++ "!"`) ist die Interpolation oft leichter lesbar und weniger fehleranfällig. In Gleam wird die String-Interpolation intern durch String-Konkatenation oder Buffer-Techniken realisiert, was effiziente Ausführung ohne manuelle Arbeit ermöglicht. Diese Art der Interpolation findet unter der Haube statt, sodass der Entwickler nicht mit der Komplexität belastet wird.

## Siehe auch
- Gleam Official Documentation: [Strings](https://gleam.run/book/tour/strings.html)
- Rust Programming Language (ähnliche Syntax): [String Formatting](https://doc.rust-lang.org/std/fmt/)
