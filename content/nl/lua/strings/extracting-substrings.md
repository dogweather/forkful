---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:57.097674-07:00
description: "Het extraheren van deelreeksen betekent het uithalen van een specifiek\
  \ stukje uit een reeks tekens. Programmeurs doen dit om specifieke gegevens binnen\u2026"
lastmod: '2024-03-13T22:44:50.926562-06:00'
model: gpt-4-0125-preview
summary: "Het extraheren van deelreeksen betekent het uithalen van een specifiek stukje\
  \ uit een reeks tekens. Programmeurs doen dit om specifieke gegevens binnen\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?
Het extraheren van deelreeksen betekent het uithalen van een specifiek stukje uit een reeks tekens. Programmeurs doen dit om specifieke gegevens binnen een grotere tekst te isoleren, te analyseren of te manipuleren.

## Hoe:
In Lua, gebruik de functie `string.sub`:

```lua
local tekst = "Hallo, Lua!"
-- 'Hallo' extraheren
print(string.sub(tekst, 1, 5)) -- Uitvoer: Hallo

-- 'Lua' grijpen
print(string.sub(tekst, 8, 11)) -- Uitvoer: Lua
```

Of krijg de laatste tekens met negatieve indices:

```lua
-- 'Lua!' van het einde plukken
print(string.sub(tekst, -4)) -- Uitvoer: Lua!
```

Gebruik patronen om te vinden en te extraheren:

```lua
local zin = "De snelle bruine vos springt"
-- Overeenkomst en 'snelle' extraheren
print(zin:match("(%a+) snelle")) -- Uitvoer: De
```

## Diepe Duik
In de beginjaren van programmeren was het behandelen van reeksen handmatig en omslachtig, vaak met lussen en voorwaarden. Lua's `string.sub` is onderdeel van zijn rijkere reeks bibliotheken, waardoor reeksmanipulatie een fluitje van een cent is. Alternatieven voor `string.sub` zijn patroonmatchen met `string.match`, wat krachtiger is maar overbodig kan zijn voor eenvoudige taken.

De `string.sub` en patroonmatching zijn gebaseerd op C-functies vanwege Lua's C-achtergrond. Vergeleken met talen zoals Python vind je in Lua geen uitgebreide standaardbibliotheek voor reeksen; het houdt vast aan het essentiële, waarbij eenvoud en efficiëntie wordt gewaardeerd. Onthoud dat indices in Lua beginnen bij 1, niet bij 0.

## Zie Ook
- Lua 5.4 Referentiehandleiding over Strings: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- 'Programmeren in Lua' (4e editie), vooral het hoofdstuk over reeksen: [www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
