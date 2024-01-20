---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

In Elm besteht das Umwandeln eines Strings in Kleinbuchstaben darin, jeden Großbuchstaben in einem gegebenen String in seinen entsprechenden Kleinbuchstaben zu verwandeln. Dies ist nützlich, um sicherzustellen, dass der Vergleich von Zeichenkette, die nicht von der Groß- und Kleinschreibung abhängig ist, immer gleich bleibt.

## So geht's:

Umwandeln von Zeichenketten in Kleinbuchstaben in Elm ist ein einfacher Prozess, der die eingebaute `String.toLower` Funktion nutzt. Hier ein einfaches Beispiel:

```Elm
module Main exposing(..)

import Html exposing (text)
import String

main =
    String.toLower "HELLLO, ELM!"
        |> text
```
Wenn Sie dieses Programm ausführen, wird die Ausgabe `"helllo, elm!"` sein.

## Tiefgehende Infos:

Das Konzept der Groß- und Kleinschreibung selbst stammt aus den Anfängen der geschriebenen Sprache. Es wurde zur Unterscheidung von Namen und Beginn eines Satzes benutzt. In modernen Programmiersprachen ist es ein wichtiges Konzept hauptsächlich aufgrund von Zeichenkettenvergleichen.

Statt `String.toLower` könnten Sie auch die `Char.toLower` Funktion benutzen und durch jeden Buchstaben in einem String iterieren. Dies kann nützlich sein, wenn Sie etwas mehr Kontrolle über den Prozess haben wollen.

Aber `String.toLower` erledigt diese Aufgabe effektiv. Intern verwendet es eine Standard-Funktion der Elm-Runtime, um jedes Zeichen in der Zeichenkette in seinen Kleinbuchstabenequivalent umzuwandeln.

## Weiterführende Links:

- [Elm String API-Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Char API-Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/Char)
- [Unicode Character Case Mapping](https://www.unicode.org/versions/Unicode13.0.0/ch03.pdf#G33992)