---
title:                "Elm: Das Löschen von Zeichen, die einem Muster entsprechen."
simple_title:         "Das Löschen von Zeichen, die einem Muster entsprechen."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung gibt es oft Situationen, in denen man bestimmte Zeichen aus einer Zeichenfolge löschen möchte. Vielleicht müssen unerwünschte Leerzeichen entfernt werden oder bestimmte Zeichenfolgen entschlüsselt werden. In solchen Fällen ist es hilfreich zu wissen, wie man Zeichen löschen kann, die einem bestimmten Muster entsprechen.

# Wie geht's

Es gibt verschiedene Möglichkeiten, um Zeichen zu löschen, die einem bestimmten Muster entsprechen. In Elm kann man zum Beispiel die Funktion "String.filter" verwenden. Hier ein Beispielcode, der alle Leerzeichen aus einer Zeichenfolge löscht:

```Elm
String.filter (\c -> c /= " ") "Hallo Welt" 
```

Die Ausgabe dieses Codes wäre "HalloWelt". Man kann auch ein reguläres Ausdrucksmuster verwenden, um bestimmte Zeichen zu löschen. Hier ein Beispiel, das alle Zahlen aus einer Zeichenfolge entfernt:

```Elm
String.filter (\c -> not (Char.isDigit c)) "123 Elm Programmierung" 
```

Die Ausgabe wäre " Elm Programmierung".

# Tiefere Einblicke

In Elm basiert die Funktion "String.filter" auf der Funktion "List.filter", die Listen bearbeitet. Diese wiederum basiert auf der Funktion "filter", die in der funktionalen Programmiersprache Haskell verwendet wird. Diese Funktion nimmt eine Funktion und eine Liste als Parameter und gibt eine neue Liste zurück, die nur die Elemente der ursprünglichen Liste enthält, für die die Funktion "True" ergibt.

Man kann auch die Funktion "String.break" verwenden, um eine Zeichenfolge in zwei Teile zu zerlegen, basierend auf einem bestimmten Muster. Zum Beispiel könnte man auf diese Weise einen Teil der Zeichenfolge entfernen und den anderen Teil behalten.

```Elm
String.break (\c -> c == " ") "Hallo Welt" 
```

Die Ausgabe wäre ein Paar von Zeichenfolgen, in diesem Fall "Hallo" und "Welt". Diese Funktionen können sehr nützlich sein, um Zeichen zu manipulieren und auf bestimmte Weise aus einer Zeichenfolge zu entfernen.

# Siehe auch

- Die offizielle Elm-Dokumentation zu String-Funktionen: https://package.elm-lang.org/packages/elm/core/latest/String
- Eine Einführung in reguläre Ausdrücke in Elm: https://guide.elm-lang.org/appendix/regex.html
- Weitere Beispiele und Anwendungsmöglichkeiten von String-Funktionen in Elm: https://medium.com/@lucamug/elm-has-a-pretty-good-string-package-6cc732b02f94