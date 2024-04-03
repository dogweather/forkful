---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:08.890775-07:00
description: "Het schrijven van een tekstbestand houdt in dat er data wordt opgeslagen\
  \ in een bestand in een leesbare vorm. Programmeurs doen dit om configuraties op\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.955531-06:00'
model: gpt-4-0125-preview
summary: Het schrijven van een tekstbestand houdt in dat er data wordt opgeslagen
  in een bestand in een leesbare vorm.
title: Een tekstbestand schrijven
weight: 24
---

## Wat & Waarom?
Het schrijven van een tekstbestand houdt in dat er data wordt opgeslagen in een bestand in een leesbare vorm. Programmeurs doen dit om configuraties op te slaan, gebruikersgegevens te bewaren of informatie te loggen voor debugging.

## Hoe:
```Lua
-- Schrijven naar een tekstbestand in Lua
local fileName = "voorbeeld.txt"
local inhoud = "Hallo, bestand!"

local bestand = io.open(fileName, "w") -- Open het bestand in schrijfmodus
if bestand then
    bestand:write(inhoud)               -- Schrijf inhoud naar het bestand
    bestand:close()                     -- Sluit het bestand altijd af als je klaar bent
else
    print("Fout bij het openen van bestand!")
end
```
Voorbeelduitvoer in `voorbeeld.txt`:
```
Hallo, bestand!
```

Het tekstbestand lezen:
```Lua
local bestand = io.open(fileName, "r") -- Open het bestand in leesmodus
if bestand then
    local bestandInhoud = bestand:read("*a") -- Lees de volledige inhoud
    print(bestandInhoud)                     -- Zet de inhoud naar de console
    bestand:close()                        -- Sluit het bestand
else
    print("Fout bij het lezen van bestand!")
end
```
Console-uitvoer:
```
Hallo, bestand!
```

## Diepere Duik
Lua's bestandsbehandelingsparadigma heeft zijn wortels in de stdio-bibliotheek van C, bekend om zijn eenvoud en flexibiliteit. In tegenstelling tot databases of binaire formaten, zijn tekstbestanden gemakkelijk te bewerken en leesbaar voor mensen zonder speciale tools. Bij het omgaan met kleinschalige gegevensopslag of eenvoudige gegevensformaten zijn tekstbestanden een geschikte keuze vanwege hun toegankelijkheid en compatibiliteit tussen verschillende platforms. Implementatiegewijs beheert Lua's `io`-bibliotheek bestandsoperaties, waaronder het openen (`io.open`), lezen (`bestand:read`), schrijven (`bestand:write`) en sluiten van bestanden (`bestand:close`).

## Zie Ook
- Lua 5.4 referentiehandleiding: https://www.lua.org/manual/5.4/
- Programmeren in Lua (4e editie): https://www.lua.org/pil/contents.html
- Vergelijking van I/O-modellen: https://www.lua.org/pil/21.2.1.html
