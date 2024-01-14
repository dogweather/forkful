---
title:                "Elm: Ermittlung der Länge eines Strings"
simple_title:         "Ermittlung der Länge eines Strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings kann eine nützliche Fähigkeit sein, wenn man mit Texten in der Programmierung arbeitet. Es kann helfen, die Effizienz des Codes zu verbessern und Fehler zu vermeiden. In diesem Blog-Beitrag werden wir uns ansehen, wie man die Länge eines Strings in Elm bestimmen kann.

## Wie geht's

Um die Länge eines Strings in Elm zu finden, können wir die `String.length` Funktion verwenden. Diese Funktion gibt die Anzahl der Zeichen im String zurück.

```Elm
import String

string = "Hallo Welt!"
length = String.length string
```

Die Variable `length` wird hier den Wert 11 haben, da der String "Hallo Welt!" aus 11 Zeichen besteht. Wir können auch Strings mit Sonderzeichen verwenden, die `String.length` korrekt verarbeiten kann.

```Elm
import String

string = "Äpfel und Birnen sind lecker 🍎🍐"
length = String.length string
```

In diesem Beispiel wird die Variable `length` den Wert 30 haben, da unser String aus 30 Zeichen besteht, einschließlich des Emoji-Symbols.

## Tiefer Einblick

Die `String.length` Funktion ist eine von vielen hilfreichen Funktionen in der Elm-Standardbibliothek, die uns bei der Arbeit mit Strings unterstützen können. Sie arbeitet effizient und ist in der Lage, Unicode-Zeichen korrekt zu zählen, unabhängig von ihrer Länge oder Komplexität. Es ist auch wichtig zu beachten, dass die Länge eines Strings immer dynamisch sein kann und sich ändern kann, abhängig davon, welche Operationen auf den String angewendet werden.

## Siehe auch

- [Offizielle Elm-Dokumentation über Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Ein Leitfaden zur Erstellung von Strings in Elm](https://guide.elm-lang.org/error_handling/strings.html)
- [Einführung in die Programmierung mit Elm](https://medium.com/@Floriangavillet/elm-tutorial-introduction-to-programming-with-the-pleasant-language-6b1479f631ff)