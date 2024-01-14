---
title:                "Elm: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung einer Zeichenfolge in Kleinbuchstaben kann nützlich sein, wenn man sicherstellen möchte, dass der Benutzer bei der Eingabe von Text keine Großbuchstaben verwendet. Dies ist besonders beim Vergleich von Eingaben wichtig.

## Wie

```Elm
import String

stringToLowerCase : String -> String
stringToLowerCase input =
    String.toLower input

```

Output:

```
stringToLowerCase "Elm ist großartig!" == "elm ist großartig!"
```

## Tiefer Einblick

Die Funktion `String.toLower` ist Teil des Elm Standardbibliothek und kann verwendet werden, um eine Zeichenfolge in Kleinbuchstaben zu konvertieren. Sie ignoriert automatisch nicht-alphabetische Zeichen und führt auch keine Trennung von Sätzen durch. Es ist wichtig zu beachten, dass die Rückgabe der Funktion eine neue kopierte Zeichenfolge ist und die ursprüngliche Zeichenfolge unverändert bleibt.

## Siehe Auch

- [Elm String Modul Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Einführung in Elm: Eine funktionale Programmiersprache für das Web](https://medium.com/@NStorandt/die-einf%C3%BChrung-in-elm-f3adff55d8df)
- [Grundlagen der Zeichenfolgenmanipulation in Elm](https://elmprogramming.com/string-basics.html)