---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei bedeutet, Daten als Text in einer neuen oder bestehenden Datei auf deinem Speichermedium zu speichern. Programmierer nutzen dies für Datensicherungen, Konfigurationsdateien oder um Dinge mit anderen Programmen und Systemen auszutauschen.

## How to:

Gleam bietet derzeit keine Standardbibliothek für das Schreiben von Dateien, da die Sprache noch jung ist. Du müsstest auf die Funktionen von Erlang zugreifen. Hier ist wie:

```gleam
// Erlang-Funktionen importieren
external fn file_write_string(String, String) -> Result(Nil, Atom) =
  "erlang" "file:write_file"

pub fn main() -> Result(Nil, Atom) {
  file_write_string("hello.txt", "Hallo Gleam Welt!")
}
```

Erwartete Ausgabe:
Eine neue Datei namens "hello.txt" enthält den Text "Hallo Gleam Welt!".

## Deep Dive:

Gleam wurde im Jahr 2018 veröffentlicht und inspiriert von Sprachen wie Rust und Elm. Für Dateioperationen wird typischerweise auf das umfangreiche Erlang-Ökosystem zurückgegriffen. Alternativ kannst du auch native Bibliotheken der Zielplattform deines Gleam-Codes verwenden.

Die Funktion `file_write_string` im obigen Beispiel nutzt die Erlang-Standardbibliothek, um eine Datei zu schreiben. Gleam ermöglicht nahtlose Interaktion mit Erlang-Code, was viele operationelle Einsatzmöglichkeiten bietet.

## See Also:

- Gleam-Website: [https://gleam.run](https://gleam.run)
- Erlang `file` Modul Dokumentation: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Erlang Interoperabilität in Gleam: [https://gleam.run/book/tour/external-functions.html](https://gleam.run/book/tour/external-functions.html)