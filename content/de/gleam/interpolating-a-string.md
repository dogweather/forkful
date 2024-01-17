---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Gleam: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Eine wichtige Aufgabe in der Programmierung ist das sogenannte "Interpolieren" von Strings. Dabei handelt es sich um die dynamische Ersetzung von Variablen oder Ausdrücken innerhalb eines Strings. Programmierer nutzen dies, um effizient und einfach Texte zu generieren, die Informationen aus verschiedenen Quellen enthalten.

## Wie geht's?

Um einen String in Gleam zu interpolieren, kannst du die "format!" Funktion verwenden. Sie erwartet zwei Parameter: einen String und eine Liste von Werten, die in den String eingefügt werden sollen. Zum Beispiel:

``` Gleam
format!("Hallo, {}! Du bist {} Jahre alt.", ["Max", 25])
```

Der Ausgang wäre:

``` Gleam
"Hallo, Max! Du bist 25 Jahre alt."
```
## Tiefer Tauchgang

Das Konzept des String-Interpolierens gibt es schon seit vielen Jahren und wird in verschiedenen Programmiersprachen, wie z.B. Python oder Ruby, verwendet. Eine Alternative zum String-Interpolieren wäre die Verwendung von String-Konkatenation. Allerdings kann dies mühsam und fehleranfällig sein, besonders bei längeren Strings.

Das String-Interpolieren wird in Gleam mithilfe von Formatierungs-Spezifikationen realisiert, die es ermöglichen, die Art und Weise der Platzhalter-Ersetzung festzulegen. Es ist auch möglich, komplexere Datentypen, wie z.B. Zahlen oder Listen, mithilfe von Formatierungs-Spezifikationen in Strings einzufügen.

## Siehe auch

Weitere Informationen zum String-Interpolieren in Gleam findest du in der offiziellen [Dokumentation](https://gleam.run/book/the-gleam-programming-language.html#string-interpolation). Eine Liste der verfügbaren Formatierungs-Spezifikationen und deren Verwendung findest du [hier](https://gleam.run/book/the-gleam-programming-language.html#formatting-specification).