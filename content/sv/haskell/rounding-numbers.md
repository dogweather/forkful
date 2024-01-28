---
title:                "Avrundning av tal"
date:                  2024-01-26T03:45:07.662267-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att avrunda tal innebär att justera dem till närmaste heltal eller angiven decimalplats. Programmerare avrundar tal för att kontrollera precision, anpassa utdata för användarpresentation eller minska beräkningskostnader för operationer med flyttal.

## Hur man gör:

Haskell använder funktionerna `round`, `ceiling`, `floor` och `truncate` från `Prelude` för avrundningsoperationer.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Avrundning till en specifik decimalplats finns inte i Prelude.
  -- Här är en anpassad funktion:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Djupdykning

Historiskt är avrundning betydande inom numerisk analys och datavetenskap eftersom det är avgörande för att minimera ackumulering av fel i beräkningar, särskilt innan representationer av flyttal standardiserades med IEEE 754.

Vad ska man avrunda till? `round` tar dig till närmaste heltal—upp eller ner. `ceiling` och `floor` avrundar alltid upp eller ner till det närmaste heltalet, medan `truncate` helt enkelt släpper decimalerna.

Alternativ till dessa funktioner kan innebära anpassad logik, som vår `roundTo`, eller du kanske drar in bibliotek (som Data.Fixed) för mer komplexa krav.

Se upp för oväntade resultat på grund av hur Haskell hanterar halvvägs-fall i `round` (den avrundar till det närmaste jämna talet).

## Se Också

- Haskell Preludes dokumentation för avrundningsfunktioner: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Haskell-wikin om flyttalsaritmetik: https://wiki.haskell.org/Floating_point_arithmetic
- IEEE 754-2008 standarden för mer om hur flyttal hanteras i många språk: https://ieeexplore.ieee.org/document/4610935
