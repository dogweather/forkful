---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings bedeutet, alle Buchstaben des Strings in Großbuchstaben umzuwandeln. Programmierer verwenden diese Methode, um zum Beispiel Überschriften einheitlich zu gestalten oder Text in Benutzeroberflächen hervorzuheben.

## So geht's:
Elm hat keine eingebaute Funktion, die direkt einen ganzen String großschreibt. Aber man kann es selbst bauen. Hier ein Beispiel, wie man eine solche Funktion schreibt und benutzt:

```Elm
import String

capitalize : String -> String
capitalize str =
    String.toUpper str

-- Anwendung der Funktion
main =
    String.toList "Hallo, Elm-Programmierer!" |> capitalize |> String.fromList
    -- Ausgabe: "HALLO, ELM-PROGRAMMIERER!"
```

## Tiefgang:
Historisch gesehen hat Elm viele Einflüsse aus funktionalen Programmiersprachen wie Haskell. Wie in Haskell, betont Elm Unveränderlichkeit und reine Funktionen, daher kennt die Sprache keine Methoden, die einen String direkt modifizieren können, wie es in anderen Sprachen üblich ist.

Eine alternative Herangehensweise, wenn man nur den ersten Buchstaben eines Wortes großschreiben will (wie bei einem Namen), wäre den String in eine Liste von Zeichen zu zerlegen, den ersten Buchstaben groß zu schreiben und dann wieder in einen String zu konvertieren.

Die Implementierungsdetails beim Großschreiben eines Strings hängen von der Funktion `String.toUpper` ab, die letztendlich auf die entsprechenden JavaScript-Funktionen zur Unicode-Verarbeitung zurückgreift, da Elm-Code im Browser als JavaScript ausgeführt wird.

## Siehe Auch:
- Elm String Dokumentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm Diskussionsforum für best practices: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
- Unicode Standard für Groß- und Kleinschreibung: [https://unicode.org/reports/tr21/](https://unicode.org/reports/tr21/)
