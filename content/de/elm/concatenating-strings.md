---
title:    "Elm: Verkettung von Zeichenfolgen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Warum
Warum sollte man sich mit der Verkettung von Zeichenfolgen beschäftigen?

Die Verkettung von Zeichenfolgen ist eine grundlegende Fähigkeit in der Programmierung, die es ermöglicht, mehrere Zeichenfolgen zu einem String zusammenzufügen. Dies ist besonders hilfreich, wenn man dynamische Inhalte auf einer Webseite erzeugen möchte oder komplexe Textformulare benötigt.

## Wie man Zeichenfolgen verkettet

Die Verkettung von Zeichenfolgen in Elm ist einfach und intuitiv. Hier ist ein einfaches Beispiel, das zwei Zeichenfolgen miteinander verbindet:

```Elm
string1 = "Hallo "
string2 = "Welt"
concatenated = string1 ++ string2
```

Das Ergebnis der Verkettung ist der String "Hallo Welt". Man kann auch mehrere Zeichenfolgen auf einmal verketten, indem man mehrere `++` Operatoren verwendet.

```Elm
string1 = "Hallo "
string2 = "an alle "
string3 = "Leser"
concatenated = string1 ++ string2 ++ string3
```

Das Ergebnis dieser Verkettung ist "Hallo an alle Leser".

## Tiefer Einblick in die Verkettung von Zeichenfolgen

In Elm gibt es neben dem `++` Operator noch weitere Möglichkeiten, Zeichenfolgen zu verkettet. Eine weitere nützliche Funktion ist `String.join`, die es ermöglicht, eine Liste von Zeichenfolgen mit einem Trennzeichen zu verbinden. Zum Beispiel:

```Elm
list = ["Elm", "ist", "super"]
concatenated = String.join " " list
```

Das Ergebnis davon ist der String "Elm ist super".

Eine weitere wichtige Sache, die es bei der Verkettung von Zeichenfolgen zu beachten gilt, ist die Performance. In Elm sind Strings unveränderlich, was bedeutet, dass sie jedes Mal, wenn eine Verkettung durchgeführt wird, einen neuen String erstellen. Bei größeren Anwendungen kann dies zu einer schlechten Performance führen, daher ist es wichtig, die Verkettungen sinnvoll zu gestalten und nicht unnötig viele davon zu verwenden.

## Siehe auch

- [Die Elm Dokumentation](https://guide.elm-lang.org/strings/concatenation.html)
- [Ein Artikel über die Performance in Elm](https://medium.com/@krisajenkins/the-problem-with-elm-strings-b13f80167060)
- [Ein Tutorial zur Verkettung von Zeichenfolgen in Elm](https://medium.com/@bijaydas/elm-strings-in-string-expressions-c522bd83bb20)