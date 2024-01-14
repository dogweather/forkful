---
title:                "Gleam: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren einer Zeichenfolge kann in vielen Situationen nützlich sein, wie zum Beispiel bei der Formatierung von Benutzereingaben oder der Erstellung von lesbaren Überschriften.

## Wie geht das
Die Gleam-Sprache bietet eine integrierte Funktion, um eine Zeichenfolge zu kapitalisieren: `String.capitalize`. Diese Funktion akzeptiert eine Zeichenfolge als Eingabe und gibt eine neue Zeichenfolge zurück, in der der erste Buchstabe großgeschrieben wird.

```Gleam
let input = "hi there"
let output = String.capitalize(input)

// output == "Hi there"
```

Um sicherzustellen, dass auch die restlichen Buchstaben in der Zeichenfolge im richtigen Format sind, gibt es auch die Funktion `String.capitalize_words`, die jede einzelne Wort in der Zeichenfolge großschreibt.

```Gleam
let input = "hello gleam programming"
let output = String.capitalize_words(input)

// output == "Hello Gleam Programming"
```

## Tiefer Einblick
Das Kapitalisieren einer Zeichenfolge kann auch manuell mit dem Modul `String` und seinen Funktionen `to_list`, `map` und `from_list` erreicht werden. Dies mag komplexer erscheinen, bietet aber mehr Flexibilität bei der Bearbeitung von Zeichenfolgen.

`to_list` konvertiert die Zeichenfolge in eine Liste von Zeichen. Mit `map` und einem Bedingungsausdruck kann dann jeder Buchstabe in der Liste überprüft und gegebenenfalls großgeschrieben werden. Schließlich wird die Liste mit `from_list` wieder in eine Zeichenfolge umgewandelt.

Eine detaillierte Erklärung dieses Prozesses findest du in der Gleam-Dokumentation.

## Siehe auch
- [Die Gleam-Dokumentation zu String-Funktionen](https://gleam.run/articles/strings/)
- [Ein Tutorial zur Arbeit mit Gleam-Strings](https://dev.to/lee_hodges/working-with-strings-in-gleam-1720)
- [Ein Beispielprojekt mit Gleam-String-Manipulationen](https://github.com/gleam-lang/gleam_string_examples)