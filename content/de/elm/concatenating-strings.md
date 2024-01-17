---
title:                "Verkettung von Zeichenketten"
html_title:           "Elm: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was wird erreicht und warum? 
Bei der Programmierung geht es darum, mehrere Strings -- also Zeichenfolgen -- miteinander zu verbinden, um eine längere Zeichenfolge zu erhalten. Das kann hilfreich sein, um zum Beispiel Texte dynamisch zu erstellen oder Dateien mit einem bestimmten Namen zu speichern.

## Wie funktioniert es:
```Elm 
module Main exposing (..)

concatString : String -> String -> String
concatString str1 str2 = str1 ++ str2

sampleOutput : String
sampleOutput = concatString "Hello " "world!" 

main : String
main = sampleOutput 
```

Die Ausgabe in diesem Fall wäre "Hello world!".

## Tiefgründiger Einblick:
Das Zusammenfügen von Strings wird auch als "String-Konkatenation" bezeichnet und ist ein häufig verwendetes Konzept in der Programmierung. Es ermöglicht es, Texte flexibel und auf verschiedene Weise zu verbinden und so den Code lesbarer und effizienter zu gestalten. Eine Alternative zur Konkatenation von Strings könnte die Verwendung von Listen sein, um Textabschnitte zu speichern und anschließend zusammenzufügen. In Elm wird die Konkatenation mit dem `++` Operator durchgeführt, der zwei Strings miteinander verbindet.

## Siehe auch:
- [Elm Dokumentation zu Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Artikel über die Verwendung von String-Konkatenation in der Programmierung](https://medium.com/@jillacion/string-concatenation-and-programming-a-match-made-in-heaven-89cc0e204661)