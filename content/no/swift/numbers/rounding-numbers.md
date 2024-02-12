---
title:                "Avrunding av tall"
date:                  2024-01-26T03:46:38.331734-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å avrunde tall betyr å tilnærme en numerisk verdi til en spesifikk presisjon, typisk for å fjerne uønskede desimaler. Programmerere avrunder for å håndtere minne, forbedre lesbarheten og møte domenespesifikke krav som valutarestriksjoner.

## Hvordan:

Swift tilbyr flere måter å avrunde tall på. Her er en smakebit:

```Swift
let original = 3.14159

// Standard avrunding
let standardRounded = round(original) // 3.0

// Avrunding til spesifikt desimaltall
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Avrunde ned
let roundedDown = floor(original) // 3.0

// Avrunde opp
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Desimal: \(decimalRounded), Ned: \(roundedDown), Opp: \(roundedUp)")
```

Utdata: `Standard: 3.0, Desimal: 3.142, Ned: 3.0, Opp: 4.0`

## Dypdykk

Historisk sett er avrunding et matematisk konsept som predaterer datamaskiner, essensielt i handel og vitenskap. Swifts `Foundation` rammeverk tilbyr omfattende avrundingsfunksjonalitet:

- `round(_: )` er den gode gamle halvopp avrunding.
- `floor(_: )` og `ceil(_: )` tar seg av retningsspesifikk avrunding.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` gir finere kontroll med avrundingsreglers enum.

Vær oppmerksom på `Decimal`-typen for presise økonomiske beregninger, som unngår flyttall-feil. Utforsk også `NSDecimalNumber` for kompatibilitet med Objective-C.

## Se Også

- IEEE-standarden for flyttallsaritmetikk (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
