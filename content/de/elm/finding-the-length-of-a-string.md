---
title:                "Elm: Die Länge eines Strings finden"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings kann sehr nützlich sein, um Information über einen bestimmten Text zu erhalten. Zum Beispiel könnte man die Länge eines Nutzernamens überprüfen oder die Anzahl der Buchstaben in einem Wort zählen. In diesem Blog-Beitrag werden wir uns ansehen, wie man dies in der funktionalen Programmiersprache Elm umsetzen kann.

## Wie gehts?

```Elm
stringLength : String -> Int
stringLength str =
    String.length str
```

Das obige Beispiel zeigt eine Funktion namens `stringLength`, die einen String als Eingabe akzeptiert und die Anzahl der Zeichen in diesem String als Ganzzahl zurückgibt. Die `String.length` Funktion in Elm gibt die Anzahl der Zeichen in einem String zurück.

Um diese Funktion aufzurufen, können wir sie einfach in unserem Code verwenden:

```Elm
stringLength "Hallo Welt" --> 11
stringLength "" --> 0
```

In diesem Beispiel bekommt die Funktion den String "Hallo Welt" und die leere Zeichenkette als Eingabe und gibt die entsprechende Anzahl von Zeichen zurück.

## Tiefentauchen

Obwohl wir bereits gesehen haben, wie einfach es ist, die Länge eines Strings in Elm zu finden, gibt es einige weitere Dinge, die man beachten sollte. Zum Beispiel behandelt die `String.length` Funktion Unicode Zeichen richtig, was bedeutet, dass sie auch mit nicht-ASCII Zeichen umgehen kann. Außerdem gibt diese Funktion immer die korrekte Anzahl von Zeichen zurück, unabhängig von der Verwendung von verschiedenen Zeichenkodierungen.

Eine weitere nützliche Funktion in Elm ist `String.indexed`, die eine Liste von Tupeln zurückgibt, die jedes Zeichen im String zusammen mit seinem Index enthält. Dadurch können wir bessere Kontrolle über eine bestimmte Position in einem String haben.

```Elm
String.indexed "Hallo Welt" --> [(0, 'H'), (1, 'a'), (2, 'l'), (3, 'l'), (4, 'o'), (5, ' '), (6, 'W'), (7, 'e'), (8, 'l'), (9, 't')]
```

## Siehe auch

- [String Module Elm Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Einführung in Elm Blog-Beitrag](https://blog.hive.fi/elm-introduction-the-ultimate-beginners-guide/)