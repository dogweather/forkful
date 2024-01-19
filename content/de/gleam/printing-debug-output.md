---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Druckausgabe für Debug-Zwecke ("Debug Output") ist eine Methode, um während der Laufzeit des Programms Daten auszugeben. Programmierer verwenden sie, um den Status des Systems besser zu verstehen und fehlerhafte Bereiche zu ermitteln.

## Wie:

Verwende Gleam's `io.println` Funktion, um Text auszugeben. Hier ist ein einfaches Beispiel:

```Gleam
import gleam/io

fn main() {
  io.println("Hallo, Gleam!")
}
```

Ausführen dieses Codes wird "Hallo, Gleam!" in der Konsole ausgeben.

Um Variablen auszugeben, nutze String Interpolation:

```Gleam
import gleam/io.{println}
import gleam/string.{from_int}

fn main() {
  let num = 5
  let msg = "Die Nummer ist " <> from_int(num)
  println(msg)
}
```

Dieser Code gibt aus: "Die Nummer ist 5"

## Tiefere Einblicke

Die Verwendung von `Debug Output` ist eine historische Praxis, die auf den Beginn der Programmierung zurückgeht. Es bietet eine einfache Möglichkeit, den Zustand eines Programms zu überwachen und zu verstehen.

Alternativ könntest du ein Logging-Framework wie Lumberjack verwenden, um ein besser konfigurierbares und komplexeres Debugging und Logging zu ermöglichen.

In Gleam Projekten, `println` ist definiert im 'io' Modul, und `from_int` ist definiert im 'string' Modul. Sie arbeiten zusammen, um eine elegante und einfache Möglichkeit zur Überwachung der Programmausgabe zu bieten.

## Siehe auch

- Documentation for Gleam’s IO module: <https://hexdocs.pm/gleam_stdlib/gleam/io/index.html>
- Documentation for Gleam’s string module: <https://hexdocs.pm/gleam_stdlib/gleam/string/index.html>
- Lumberjack Logging Framework: <https://hexdocs.pm/lumberjack/Lumberjack.html>