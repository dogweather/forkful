---
title:                "String in Großbuchstaben umwandeln"
html_title:           "Elm: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist ein nützliches Werkzeug, um Texte auf einfache Weise zu formatieren. Es kann helfen, die Lesbarkeit von Daten und Texten zu verbessern, indem es die Großschreibung der Wörter vereinheitlicht.

## How To

Die Elm-Standardbibliothek bietet eine einfache Möglichkeit, Strings zu kapitalisieren. Dazu verwenden wir die Funktion `String.toUpper`, die jeden Buchstaben eines Strings in Großbuchstaben umwandelt.

```
Elm-Code:
String.toUpper "hallo"
    
Output:
"HALLO"
```

Wenn wir möchten, dass nur der erste Buchstabe eines Strings großgeschrieben wird, können wir die Funktion `String.capitalize` verwenden.

```
Elm-Code:
String.capitalize "hallo welt"
    
Output:
"Hallo welt"
```

## Deep Dive

Die `String.toUpper`-Funktion verwendet den ASCII-Zeichensatz, um die Großschreibung der Buchstaben zu ändern. Das bedeutet, dass nicht-englische Buchstaben möglicherweise nicht korrekt konvertiert werden. Um dieses Problem zu lösen, können wir die `String.map`-Funktion verwenden, um einen Benutzerdefinierten ASCII-Tabelle zu erstellen und sie dann mit der `String.map`-Funktion zu verwenden.

```
Elm-Code:
String.toUpperWithCustomTable "hallo" customTable

customTable =
  [ #[a] = "A"
  , #[b] = "B"
  , #[c] = "C"
  ...
  ]
    
Output:
"HALLÖ"
```

## Siehe auch

- [Elm-Dokumentation zu Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-Tutorial: Grundlagen der String-Manipulation](https://guide.elm-lang.org/strings/)
- [String-Funktionen in der Elm-Standardbibliothek](https://package.elm-lang.org/packages/elm/core/latest/String#functions)