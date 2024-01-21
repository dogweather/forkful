---
title:                "Textdatei einlesen"
date:                  2024-01-20T17:54:24.013375-07:00
model:                 gpt-4-1106-preview
simple_title:         "Textdatei einlesen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Einlesen einer Textdatei ermöglicht es Programmen, schriftliche Daten zu verarbeiten, die auf einem Speichermedium gespeichert sind. Programmierer nutzen diese Funktion, um Einstellungen zu laden, Daten zu analysieren oder Inhalte für die Weiterverarbeitung zu importieren.

## Anleitung:

Um eine Textdatei in Gleam zu lesen, nutzen wir die `gleam/io` Bibliothek. Hier ein einfaches Beispiel:

```gleam
import gleam/io
import gleam/result.{Result, Ok, Error}

// Diese Funktion liest den Inhalt einer Textdatei.
pub fn read_file(path: String) -> Result(String, String) {
  try file = io.open(path)
  try content = io.read_to_string(file)
  content
}

pub fn main() {
  let result = read_file("beispiel.txt")
  case result {
  | Ok(content) -> io.println("Dateiinhalt: " ++ content)
  | Error(_) -> io.println("Datei konnte nicht gelesen werden.")
  }
}
```

Beispiel-Ausgabe beim erfolgreichen Lesen der Datei:

```
Dateiinhalt: Hallo, Gleam!
```

## Tiefgang:

Das Lesen von Textdateien ist ein grundlegendes Konzept, das seit den frühesten Tagen der Computerprogrammierung existiert. Alternativ zum direkten Lesen von Dateien können auch Datenbanken oder andere Datenquellen genutzt werden, die mehr Funktionen bieten. In Gleam wird zur Fehlerbehandlung oft das Resultat-Muster verwendet, das aus anderen funktionalen Sprachen wie Rust oder Haskell bekannt ist. Dabei wird der Typ `Result` verwendet, um den Erfolg (Ok) oder Misserfolg (Error) einer Operation darzustellen.

## Siehe Auch:

- Erlang IO documentation for more context: [http://erlang.org/doc/apps/stdlib/io_protocol.html](http://erlang.org/doc/apps/stdlib/io_protocol.html)
- Gleam's official book which covers error handling: [https://gleam.run/book](https://gleam.run/book)