---
title:                "Haskell: Löschen von Zeichen mit einem bestimmten Muster"
simple_title:         "Löschen von Zeichen mit einem bestimmten Muster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist ein hilfreiches Werkzeug, um Daten zu bereinigen oder zu formatieren. Durch das Entfernen unerwünschter Zeichen können Fehler in den Daten vermieden werden, die zu Fehlern in der weiteren Verarbeitung führen könnten.

## Wie geht man vor?

Um Zeichen in Haskell zu löschen, gibt es zwei Hauptansätze: die Verwendung der `filter` Funktion oder die Verwendung von regulären Ausdrücken mit der `subRegex` Funktion aus dem `Text.Regex` Modul.

1. Verwendung der `filter` Funktion:

Die `filter` Funktion ermöglicht es uns, eine Liste von Elementen aufgrund eines bestimmten Prädikats zu filtern. In diesem Fall erstellen wir eine Funktion, die überprüft, ob ein bestimmtes Zeichen dem Muster entspricht und geben die Liste ohne diese Zeichen zurück.

```Haskell
deleteMatchingChar :: String -> String -> String
deleteMatchingChar pattern str = filter (\x -> not (x `elem` pattern)) str

-- Beispiel
ghci> deleteMatchingChar "aeiou" "Informatik"
"nfrmkt"
```

2. Verwendung von regulären Ausdrücken mit `Text.Regex`:

Das `Text.Regex` Modul ermöglicht uns die Verwendung von regulären Ausdrücken in Haskell. Mit der `subRegex` Funktion können wir alle Vorkommen eines bestimmten Musters durch einen leeren String ersetzen.

```Haskell
import Text.Regex

deleteMatchingChar :: String -> String -> String
deleteMatchingChar pattern str = subRegex (mkRegex pattern) str ""

-- Beispiel
ghci> deleteMatchingChar "[aeiou]" "Informatik"
"nfrmkt"
```

## Tiefere Einblicke

Das Löschen von Zeichen anhand eines Musters kann auch komplexer gestaltet werden, indem man nicht nur einzelne Zeichen, sondern ganze Wörter oder Sätze löscht. Hierfür können reguläre Ausdrücke mit Platzhaltern wie `*` oder `+` verwendet werden. Auch das Zusammenspiel mit anderen Funktionen, wie z.B. `map`, kann uns helfen, die Daten auf andere Weise zu formatieren oder zu bereinigen.

## Siehe auch

- [Die `filter` Funktion in Haskell](https://wiki.haskell.org/Filter)
- [Das `Text.Regex` Modul in Haskell](https://hackage.haskell.org/package/regex-base/docs/Text-Regex.html)