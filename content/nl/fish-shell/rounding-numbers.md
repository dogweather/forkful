---
title:                "Afronden van getallen"
date:                  2024-01-28T22:06:30.968097-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Afronden van getallen gaat over het afsnijden van decimalen om je gegevens te vereenvoudigen of aan specifieke formaten te voldoen. Programmeurs doen dit voor gebruikersvriendelijke weergave, efficiënte opslag, of wanneer decimale precisie geen issue is.

## Hoe:
In Fish hangt het afronden van getallen af van het `math` commando. Gebruik `math -s0` om af te ronden naar het dichtstbijzijnde gehele getal.

```fish
# Naar boven afronden
echo (math -s0 "4.7")
# Uitvoer: 5

# Naar beneden afronden
echo (math -s0 "4.3")
# Uitvoer: 4

# Afronden op twee decimalen
echo (math -s2 "4.5678")
# Uitvoer: 4.57

# Negatief getal afronden
echo (math -s0 "-2.5")
# Uitvoer: -3
```

## Diepere Duik
Historisch gezien werd het afronden van getallen meer handmatig gedaan of met externe hulpmiddelen, maar in moderne shells zoals Fish is het ingebakken in de ingebouwde hulpprogramma's. Fish's benadering met het `math` commando vereenvoudigt zaken in vergelijking met oudere shells. Alternatieven in andere programmeeromgevingen variëren; talen zoals Python gebruiken functies zoals `round()`, terwijl Bash mogelijk meer complexe uitdrukkingen of de `bc`-hulpprogramma vereist. Fish's implementatie van afronden vereenvoudigt het scripten door de wiskunde binnen de shellomgeving te houden in plaats van andere hulpmiddelen of talen aan te roepen.

## Zie Ook
- Fish documentatie voor het `math` commando: https://fishshell.com/docs/current/cmds/math.html
- IEEE Standaard voor zwevendekommagetallen (IEEE 754): https://ieeexplore.ieee.org/document/4610935
