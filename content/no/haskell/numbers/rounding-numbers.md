---
date: 2024-01-26 03:44:48.160787-07:00
description: 'Hvordan: Haskell bruker funksjonene `round`, `ceiling`, `floor` og `truncate`
  fra `Prelude` for avrundingsoperasjoner.'
lastmod: '2024-03-13T22:44:40.836227-06:00'
model: gpt-4-0125-preview
summary: Haskell bruker funksjonene `round`, `ceiling`, `floor` og `truncate` fra
  `Prelude` for avrundingsoperasjoner.
title: Avrunding av tall
weight: 13
---

## Hvordan:
Haskell bruker funksjonene `round`, `ceiling`, `floor` og `truncate` fra `Prelude` for avrundingsoperasjoner.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Å runde til et spesifikt desimalpunkt er ikke i Prelude.
  -- Her er en tilpasset funksjon:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Dypdykk
Historisk sett er avrunding viktig i numerisk analyse og informatikk fordi det er avgjørende for å minimere oppsamling av feil i beregninger, spesielt før flyttallsrepresentasjoner ble standardisert med IEEE 754.

Hva skal man runde til? `round` tar deg til nærmeste heltall—opp eller ned. `ceiling` og `floor` runder alltid opp eller ned til nærmeste heltall, mens `truncate` bare dropper desimalpunktene.

Alternativer til disse funksjonene kan innebære tilpasset logikk, som vår `roundTo`, eller du kan dra inn biblioteker (som Data.Fixed) for mer komplekse behov.

Pass deg for uventede resultater på grunn av hvordan Haskell håndterer halvveis tilfeller i `round` (den runder til det nærmeste partallet).

## Se Også
- Haskell Prelude-dokumentasjon for avrundingsfunksjoner: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Haskell Wiki om flyttallsaritmetikk: https://wiki.haskell.org/Floating_point_arithmetic
- IEEE 754-2008-standard for mer om hvordan flyttall håndteres i mange språk: https://ieeexplore.ieee.org/document/4610935
