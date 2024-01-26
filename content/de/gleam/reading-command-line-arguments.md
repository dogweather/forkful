---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:56:05.833926-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente sind Eingaben, die ein Programm beim Start aus der Konsole erhält. Programmierer nutzen diese, um das Verhalten von Programmen variabel zu gestalten ohne den Code ändern zu müssen.

## How to:
Gleam macht es einfach, auf Kommandozeilenargumente zuzugreifen. Hier ist ein schnelles Beispiel:

```Gleam
import gleam/io

fn main(args: List(String)) {
  let greeting = args
    |> list.head
    |> result.unwrap_or("Hello")

  io.println(greeting)
}
```

Würden Sie dieses Programm so ausführen: `./mein_programm Hallo`, wäre die Ausgabe `Hallo`.

## Vertiefung
In früheren Programmiersprachen war das Lesen von Kommandozeilenargumenten oft fehleranfällig und komplex. Die modernen Sprachen, wie Gleam, verbergen viele dieser Komplexitäten in Bibliotheken und bieten ergonomische APIs. Als Alternative könnten in anderen Umgebungen auch Umgebungsvariablen oder Konfigurationsdateien genutzt werden, aber Kommandozeilenargumente bieten schnellen und direkten Zugriff auf Programmfunktionen für den Benutzer. In Gleam wird das Ergebnis direkt als Liste von Strings weitergegeben, womit einfache sowie komplexe Argumente verarbeitet werden können.

## Siehe Auch
- Gleam's offizielle Dokumentation zum Umgang mit Kommandozeilenargumenten: https://gleam.run/book/tour/cli-arguments.html
- Ein Tutorial zur Fehlerbehandlung in Gleam, wenn man mit externen Inputs arbeitet: https://gleam.run/book/tour/error-handling.html
- Diskussionen und Beispiele auf Exercism für Gleam CLI-Anwendungen: https://exercism.io/tracks/gleam/exercises
