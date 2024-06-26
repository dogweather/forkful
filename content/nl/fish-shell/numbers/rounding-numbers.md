---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:30.968097-07:00
description: 'Hoe: In Fish hangt het afronden van getallen af van het `math` commando.
  Gebruik `math -s0` om af te ronden naar het dichtstbijzijnde gehele getal.'
lastmod: '2024-03-13T22:44:51.239973-06:00'
model: gpt-4-0125-preview
summary: In Fish hangt het afronden van getallen af van het `math` commando.
title: Afronden van getallen
weight: 13
---

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
