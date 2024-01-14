---
title:    "Gleam: Substrings extrahieren"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist ein häufig verwendetes Konzept in der Programmierung und kann in vielen Situationen nützlich sein. Egal ob Sie eine Textverarbeitungssoftware erstellen oder Daten analysieren möchten, das Extrahieren von Teilstrings ermöglicht es Ihnen, auf spezifische Informationen zuzugreifen und sie zu manipulieren.

## Wie man vorgeht

Das Extrahieren von Teilstrings kann mit der `substring` Funktion in Gleam einfach realisiert werden. Diese Funktion erwartet zwei Argumente: einen String und eine Range, die den zu extrahierenden Teil des Strings festlegt. Zum Beispiel:

```Gleam
let text = "Gleam ist eine tolle Sprache für funktionale Programmierung"
let substr = substring(text, 6..10)
```

In diesem Beispiel wird der Teilstring "ist" aus dem ursprünglichen Text extrahiert und in der Variable `substr` gespeichert. Das Ergebnis ist ein neuer String mit dem Wert "ist".

## Tiefere Einblicke

Es gibt einige nützliche Dinge, die Sie beim Extrahieren von Teilstrings beachten sollten. Zum einen können Sie auch die Zeichenlänge angeben, anstatt eine Range zu verwenden. So können Sie beispielsweise die ersten 5 Zeichen eines Strings extrahieren, indem Sie `5..` als Range angeben. Darüber hinaus können Sie auch negative Zahlen verwenden, um vom Ende des Strings aus zu zählen. Wenn Sie beispielsweise `-3..` angeben, werden die letzten 3 Zeichen des Strings extrahiert.

Ein weiteres interessantes Konzept ist die Verwendung von Pattern Matching, um Teilstrings zu extrahieren. Auf diese Weise können Sie bestimmte Muster innerhalb eines Strings erkennen und extrahieren. Zum Beispiel:

```Gleam
let text = "Apples, Bananas, Oranges"
let ["Apples", _] = string.split(text, ", ")
```

In diesem Beispiel wird mittels Pattern Matching der erste Teilstring "Apples" extrahiert und in der Variable `apples` gespeichert. Der zweite Teil des Ausdrucks `_` zeigt an, dass der zweite Teilstring übersprungen wird und nicht benötigt wird.

## Siehe auch

- [Gleam Dokumentation über Substrings](https://gleam.run/documentation/stdlib.html#substring)
- [String-Operationen in Gleam](https://gleam.run/documentation/strings.html)
- [Pattern Matching in Gleam](https://gleam.run/documentation/pattern_matching.html)