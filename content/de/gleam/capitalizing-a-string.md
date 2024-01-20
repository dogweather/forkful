---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Großschreibung in Gleam: Ein kurzer Leitfaden

## Was & Warum?
Großschreibung verwandelt alle Buchstaben eines Strings in Großbuchstaben. Es ist nützlich für einheitliche Darstellung, wie Titel, oder wenn man Text hervorheben will.

## How to:
In Gleam gibt es standardmäßig keine eingebaute Funktion für die String-Großschreibung. Daher muss man sich mit Listen und Char-Codes behelfen:

```gleam
import gleam/list
import gleam/char

pub fn capitalize_string(str: String) -> String {
  str
  |> String.to_graphemes
  |> list.map(char.uppercase)
  |> list.fold([], fn(x, acc) -> acc ++ x)
  |> String.from_slice
}

fn main() {
  let my_string = "hallo, Gleam!"
  let capitalized = capitalize_string(my_string)
  io.debug(capitalized)  // Output: "HALLO, GLEAM!"
}
```

## Deep Dive
In älteren oder niedrigeren Programmiersprachen ist das Großschreiben oft ein eingebauter String-Operation. Das Gleam allerdings eine jüngere Sprache ist, fokussiert es sich mehr auf Sicherheit und Robustheit, und überlässt solche Funktionalitäten oft externen Libraries oder dem Entwickler selbst.

Man könnte beim Kapitalisieren auch Unicode und locale beachten, was die Implementierung komplexer macht – ein Grund, warum es nicht einfach in Gleam eingebaut ist.

Alternativen:

- Benutze Erlang- oder Elixir-Bibliotheken: Da Gleam auf der BEAM VM läuft, kann man vorhandene Bibliotheken nutzen.
- Schreibe eine eigene Gleam-Bibliothek: Eine Chance, zur Gleam-Community beizutragen.

## See Also
- Gleam's official documentation: [Gleam Language](https://gleam.run/)
- Unicode Character Case Mapping: [Unicode Standard](https://www.unicode.org/versions/Unicode13.0.0/ch03.pdf)
- Elixir String Capitalization: [Elixir hexdocs](https://hexdocs.pm/elixir/String.html)