---
title:    "Haskell: Zwei Daten vergleichen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein wichtiger Bestandteil der Programmierung. Es ermöglicht uns, Bedingungen zu erstellen und bestimmte Aktionen auszuführen, je nachdem, ob eine Bedingung erfüllt ist oder nicht. Mit Hilfe von Haskell können wir dies auf elegante und funktionale Weise tun.

## Wie geht man vor

Um zwei Daten in Haskell zu vergleichen, müssen wir zuerst sicherstellen, dass sie das gleiche Datentyp haben. Wir können dies mithilfe der `compare` Funktion tun, die zwei Werte desselben Typs vergleicht und uns entweder "LT" (Less Than), "GT" (Greater Than) oder "EQ" (Equal) zurückgibt.

```Haskell
-- Vergleichen von Ints
-- Output: LT
compare 5 10 

-- Vergleichen von Strings
-- Output: EQ
compare "Haskell" "Haskell"
```

Wir können auch die `==`, `<`, `>` Operatoren verwenden, um zwei Daten zu vergleichen. Diese operieren direkt auf Werten und nicht auf Funktionen und geben uns ebenfalls einen boolschen Wert zurück.

```Haskell
-- Vergleichen von Floats mit dem "<" Operator
-- Output: True
3.14 < 4.13
```

## Tieferer Einblick

Bei der Verwendung von `compare` ist es wichtig zu wissen, dass das zurückgegebene Ergebnis eine Instanz der `Ordering` Datenklasse ist. Diese ordnet die Ergebnisse `LT`, `GT` und `EQ` in dieser Reihenfolge an. In einigen Fällen kann es auch nützlich sein zu wissen, dass `EQ` als 0, `LT` als -1 und `GT` als 1 interpretiert wird.

```Haskell
-- Vergleichen von Daten mithilfe von Ordering
-- Output: GT
5 `compare` 4.5
```

## Siehe auch

- [Haskell-Dokumentation zur `compare` Funktion](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:compare)
- [Video-Tutorial zur Verwendung von Vergleichsoperatoren in Haskell](https://www.youtube.com/watch?v=TOgV0oeejZo)
- [Informationen zur `Ordering` Datenklasse](https://wiki.haskell.org/Class)