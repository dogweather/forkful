---
title:                "Verkettung von Zeichenfolgen"
html_title:           "Haskell: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Bei der Konkatenation von Zeichenfolgen geht es darum, zwei oder mehr Strings zusammenzufügen, um einen einzigen zusammengesetzten String zu erstellen. Programmierer nutzen dies, um zum Beispiel Wörter zusammenzufügen oder Texte dynamisch zu generieren.

## Anleitung:
```Haskell
"Hello" ++ " " ++ "World!" 
-- Ausgabe: "Hello World!"
"2" ++ " + " ++ "2" ++ " = " ++ "4" 
-- Ausgabe: 2 + 2 = 4
```

## Tiefergehende Informationen:
Die Idee der String-Konkatenation existiert schon seit den Anfängen der Programmierung. In Haskell kann dies mithilfe des Operators "++" oder durch die Funktion "concat" erreicht werden. Alternativ können von Strings auch Listen erstellt werden und diese per "++" verbunden werden. Die Implementierung von String-Konkatenation hängt vom konkreten Sprach- und Compiler-Design ab, in Haskell erfolgt dies jedoch in einer effizienten Art und Weise.

## Siehe auch: 
- ["Arbeiten mit Strings" im Haskell Wiki](https://wiki.haskell.org/Working_with_strings)
- ["Strings in Haskell" auf haskell.org](https://www.haskell.org/tutorial/strings.html)