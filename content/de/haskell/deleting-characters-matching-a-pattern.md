---
title:    "Haskell: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Löschung von Zeichen, die einem bestimmten Muster entsprechen, kann für Programmiererinnen und Programmierer nützlich sein, um unerwünschte Zeichen aus einer Zeichenkette zu entfernen oder bestimmte Datenstrukturen zu filtern.

## So geht's

In Haskell gibt es mehrere Methoden, um Zeichen in einer Zeichenkette basierend auf einem bestimmten Muster zu löschen. Hier sind zwei Beispiele, die das Löschen von Bindestrichen aus einer Telefonnummer demonstrieren:

```Haskell
-- Verwenden von `filter` Funktion
deleteHyphens :: String -> String
deleteHyphens str = filter (\x -> x /= '-') str
deleteHyphens "555-123-4567" -- "5551234567"

-- Verwenden von `map` und `concat` Funktionen
deleteHyphens' :: String -> String
deleteHyphens' str = concat $ map (\x -> if x == '-' then "" else [x]) str
deleteHyphens' "555-123-4567" -- "5551234567"
```

Das erste Beispiel verwendet die `filter` Funktion, um alle Bindestriche in der Zeichenkette zu entfernen. Das zweite Beispiel verwendet die `map` Funktion, um jedes Zeichen in der Zeichenkette zu überprüfen und wenn es ein Bindestrich ist, wird es durch einen leeren String ersetzt.

## Tiefergehende Analyse

Beim Löschen von Zeichen basierend auf einem Muster ist es wichtig zu beachten, dass String in Haskell eine Liste von Buchstaben ist. Daher können dieselben Funktionen, die auf Listen angewendet werden, auch auf Zeichenketten angewendet werden. Beim Löschen von Zeichen müssen wir jedoch überprüfen, ob das zu löschende Zeichen mit dem gegebenen Muster übereinstimmt. Hier wird die anonyme Funktion `\x -> x /= '-'` verwendet, um zu überprüfen, ob das aktuelle Zeichen nicht mit einem Bindestrich übereinstimmt.

## Siehe auch

- Dokumentation zu `filter`: https://hackage.haskell.org/package/base/docs/Data-List.html#v:filter
- Dokumentation zu `map`: https://hackage.haskell.org/package/base/docs/Prelude.html#v:map