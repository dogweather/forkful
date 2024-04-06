---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:06.815827-07:00
description: 'Hoe: Functies worden complexer, met verschillende taken.'
lastmod: '2024-04-05T21:53:50.965555-06:00'
model: gpt-4-0125-preview
summary: Functies worden complexer, met verschillende taken.
title: Code organiseren in functies
weight: 18
---

## Hoe:
```Lua
-- Definieer een eenvoudige functie om te groeten
function greet(name)
    return "Hallo, " .. name .. "!"
end

-- Gebruik de functie
print(greet("Lua Programmeur")) -- Voorbeelduitvoer: Hallo, Lua Programmeur!
```

Functies worden complexer, met verschillende taken:
```Lua
-- Een functie om de oppervlakte van een rechthoek te berekenen
function calculateArea(width, height)
    return width * height
end

-- Roep de functie aan en print het resultaat
local area = calculateArea(5, 4)
print(area)  -- Voorbeelduitvoer: 20
```

## Diepere Duik
Sinds de oprichting in de jaren 90 heeft Lua modulair ontwerp gestimuleerd. Code organiseren met functies is niet uniek voor Lua—het wordt al beoefend sinds de dageraad van programmeertalen zoals Fortran en Lisp. Alternatieven zoals inline code en het kopiëren en plakken van dezelfde code over en over zijn niet alleen afgekeurd; ze zijn potentiële foutnesten.

In Lua zijn functies burgers van de eerste klasse, wat betekent dat ze in variabelen kunnen worden opgeslagen, als argumenten kunnen worden doorgegeven en uit andere functies kunnen worden geretourneerd. Ze zijn veelzijdig. De single-threaded aard van Lua betekent dat je functies slank en krachtig moet houden voor prestaties. Functies kunnen lokaal (gescopeerd) of globaal zijn, en begrijpen wanneer elk te gebruiken kan de efficiëntie van je script maken of breken.

## Zie Ook
- Officiële Lua-documentatie over functies: https://www.lua.org/pil/6.html
- Praktijkvoorbeelden van functiegebruik in Lua: https://lua-users.org/wiki/SampleCode
- Schone code praktijken in Lua: https://github.com/Olivine-Labs/lua-style-guide
