---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:21.407453-07:00
description: 'Hoe: In Lua gebruiken we `os.date` om datums naar strings te formatteren.
  Hier is een stukje code om op te kauwen.'
lastmod: '2024-03-13T22:44:50.948786-06:00'
model: gpt-4-0125-preview
summary: In Lua gebruiken we `os.date` om datums naar strings te formatteren.
title: Een datum converteren naar een string
weight: 28
---

## Hoe:
In Lua gebruiken we `os.date` om datums naar strings te formatteren. Hier is een stukje code om op te kauwen.

```lua
local now = os.time()
local geformatteerd = os.date("%Y-%m-%d %H:%M:%S", now)
print(geformatteerd)
-- Voorbeelduitvoer: 2023-04-01 15:24:37
```

Wil je een andere smaak? Pas het stringpatroon aan.

```lua
local vriendelijk_formaat = os.date("%B %d, %Y")
print(vriendelijk_formaat)
-- Voorbeelduitvoer: April 01, 2023
```

## Diepere Duik
Lua's `os.date` functie is gemodelleerd naar de POSIX `strftime` functie. Als je goed kijkt, zul je merken dat het lijkt op de `printf` familie van C—dezelfde wortels.

Alternatieven? Zeker. Je zou kunnen worstelen met stringconcatenatie en indexering van tabellen—handmatig datumdelen pakken. Maar waarom zou je je in het zweet werken als `os.date` het voor je regelt?

Implementatie details? De `os.date` functie kan op twee manieren werken:
- Gegeven een formaatstring, retourneert het de geformatteerde datum.
- Laat het formaat weg, en het retourneert een tabel met datumcomponenten.

Leuk feitje: Lua's tijdgerelateerde functies gebruiken het tijdperk als referentie—het aantal seconden sinds 1 januari 1970. Deze eigenaardigheid gaat terug op de Unix-tijd.

## Zie Ook
- Lua's referentiehandleiding over `os.date`: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
- strftime formaatspecificaties om `os.date` op te leuken: http://strftime.org/
- Een duik in Unix-epochtijd voor de nieuwsgierigen: https://en.wikipedia.org/wiki/Unix_time
