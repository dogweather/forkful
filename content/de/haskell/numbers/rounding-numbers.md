---
date: 2024-01-26 03:44:54.593241-07:00
description: "Das Runden von Zahlen bedeutet, sie auf die n\xE4chstgelegene ganze\
  \ Zahl oder eine angegebene Dezimalstelle anzupassen. Programmierer runden Zahlen,\
  \ um die\u2026"
lastmod: '2024-03-13T22:44:53.925873-06:00'
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen bedeutet, sie auf die n\xE4chstgelegene ganze Zahl\
  \ oder eine angegebene Dezimalstelle anzupassen. Programmierer runden Zahlen, um\
  \ die\u2026"
title: Zahlen runden
weight: 13
---

## Was & Warum?

Das Runden von Zahlen bedeutet, sie auf die nächstgelegene ganze Zahl oder eine angegebene Dezimalstelle anzupassen. Programmierer runden Zahlen, um die Genauigkeit zu steuern, Ausgaben für die Benutzerdarstellung anzupassen oder die Berechnungskosten für Gleitkommaoperationen zu reduzieren.

## Wie:

Haskell verwendet die Funktionen `round`, `ceiling`, `floor` und `truncate` aus dem `Prelude` für Rundungsoperationen.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Runden auf eine spezifische Dezimalstelle ist nicht in Prelude enthalten.
  -- Hier ist eine benutzerdefinierte Funktion:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Vertiefung

Historisch gesehen ist das Runden in der numerischen Analyse und Informatik von Bedeutung, da es entscheidend ist, die Akkumulation von Fehlern in Berechnungen zu minimieren, insbesondere bevor Gleitkommadarstellungen mit IEEE 754 standardisiert wurden.

Worauf soll gerundet werden? `round` bringt Sie zur nächstgelegenen ganzen Zahl - nach oben oder unten. `ceiling` und `floor` runden immer auf bzw. ab zur nächsten ganzen Zahl, während `truncate` einfach die Dezimalstellen fallen lässt.

Alternativen zu diesen Funktionen könnten benutzerdefinierte Logik beinhalten, wie unsere `roundTo`, oder man könnte Bibliotheken (wie Data.Fixed) für komplexere Anforderungen einbeziehen.

Achten Sie auf unerwartete Ergebnisse aufgrund der Behandlung von Halbwegsfällen durch Haskell in `round` (es rundet auf die nächste gerade Zahl).

## Siehe auch

- Haskell Prelude Dokumentation für Rundungsfunktionen: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Das Haskell Wiki zur Gleitkommadarstellung: https://wiki.haskell.org/Floating_point_arithmetic
- IEEE 754-2008 Standard für mehr darüber, wie Gleitkomma in vielen Sprachen gehandhabt wird: https://ieeexplore.ieee.org/document/4610935
