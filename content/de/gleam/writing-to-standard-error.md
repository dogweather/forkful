---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben auf Standard Error (stderr) bedeutet, Fehlermeldungen und Diagnoseinformationen in einem separaten Kanal auszugeben. Programmierer nutzen es, um Fehlernachrichten vom regulären Programmoutput zu trennen.

## Anleitung:
```gleam
import gleam/io

pub fn main() {
  io.stderr("Etwas ist schiefgelaufen!\n")
}
```

Ausgabe:
```
Etwas ist schiefgelaufen!
```

## Tiefer Einblick:
Das Konzept von Standard Error stammt aus der Unix-Welt und ist ein separates Ausgabestream neben Standard Output (stdout). Alternativen wie das Protokollieren in eine Datei sind ebenfalls verbreitet, aber stderr ermöglicht es Tools wie Shell, Fehler leicht zu erkennen und darauf zu reagieren. In Gleam wird stderr über die `io` Bibliothek implementiert, welche die unterliegenden Erlang-Funktionen nutzt.

## Siehe Auch:
- [Erlang's error_logger für komplexere Logs](http://erlang.org/doc/man/error_logger.html)
