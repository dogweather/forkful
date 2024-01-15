---
title:                "Herausziehen von Teilzeichenketten"
html_title:           "Gleam: Herausziehen von Teilzeichenketten"
simple_title:         "Herausziehen von Teilzeichenketten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Substrings kann hilfreich sein, wenn man aus einer größeren Zeichenfolge bestimmte Teilstrings herausfiltern möchte. Zum Beispiel kann man so nur die Telefonnummer aus einer Adresszeile extrahieren oder eine URL aus einem Textabschnitt herausfinden.

## Wie geht's

Die Gleam-Standardbibliothek bietet die Funktion `Substring.slice` an, um Substrings zu extrahieren. Diese Funktion nimmt zwei Argumente: die ursprüngliche Zeichenfolge und einen Bereich, der angibt, welcher Teil extrahiert werden soll. Der Bereich wird in Form eines Tupels angegeben, das die Anfangs- und Endposition des gewünschten Substrings enthält.

```Gleam
// Definiere eine Zeichenfolge
let text = "Hallo, mein Name ist Max Mustermann!"

// Extrahiere den Teilstring "Max Mustermann" aus der Zeichenfolge
Substring.slice(text, (18, 32))

// Output: Max Mustermann
```

Bei der Indexierung der Zeichenfolge werden die ersten beiden Zeichen als 0 und 1 gezählt. Das heißt, der erste Buchstabe hat den Index 0 und der letzte Buchstabe den Index der Länge der Zeichenfolge minus 1.

Man kann auch negative Indizes verwenden, um von hinten zu zählen. Dabei entspricht der Index -1 dem letzten Zeichen, -2 dem vorletzten Zeichen und so weiter.

```Gleam
// Definiere eine Zeichenfolge
let text = "Hallo, mein Name ist Max Mustermann!"

// Extrahiere den Teilstring "Max Mustermann" aus der Zeichenfolge
Substring.slice(text, (-15, -1))

// Output: Max Mustermann
```

Um sicherzustellen, dass es keine Fehler gibt, sollte man vor dem Extrahieren die Länge der Zeichenfolge überprüfen. Wenn man sichergehen möchte, dass die angegebenen Indizes innerhalb des erwarteten Bereichs liegen, kann man die Funktion `Substring.length` verwenden.

```Gleam
// Definiere eine Zeichenfolge
let text = "Hallo, mein Name ist Max Mustermann!"

// Überprüfe die Länge der Zeichenfolge
let length = Substring.length(text)

// Extrahiere den Teilstring "Max Mustermann" aus der Zeichenfolge
if length >= 32 {
  Substring.slice(text, (18, 32))
} else {
  "Text ist zu kurz."
}

// Output: Max Mustermann
```

## Eintauchen

Um Substrings effektiv zu extrahieren, ist es wichtig, die Indexierung von Zeichenfolgen zu verstehen. In Gleam beginnen Indizes immer bei 0 und enden bei der Länge der Zeichenfolge minus 1. Negative Indizes zählen immer von hinten.

Außerdem sollte man darauf achten, immer die Länge der Zeichenfolge zu überprüfen, um Fehler zu vermeiden.

## Siehe auch

- [Gleam-Standardbibliothek: Substring](https://gleam.run/modules/gleam_stdlib/string.html#fn.slice)
- [Gleam-Dokumentation: Zeichenfolgen](https://gleam.run/book/tour/strings.html)
- [Gleam-Kurs: Lektion 3 - Zeichenfolgen](https://gleam.run/courses/getting_started/lessons/strings.html)