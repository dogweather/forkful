---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:00.354951-07:00
description: "Hoe: Lua heeft geen ingebouwde datum-parser, maar je kunt de klus klaren\
  \ met `os.time` en patroonherkenning. Stel je hebt een datum als string `date_str`\u2026"
lastmod: '2024-03-13T22:44:50.946853-06:00'
model: gpt-4-0125-preview
summary: Lua heeft geen ingebouwde datum-parser, maar je kunt de klus klaren met `os.time`
  en patroonherkenning.
title: Een datum uit een string parsen
weight: 30
---

## Hoe:
Lua heeft geen ingebouwde datum-parser, maar je kunt de klus klaren met `os.time` en patroonherkenning. Stel je hebt een datum als string `date_str` en je wilt het omzetten naar een tabel die Lua kan verwerken:

```lua
local date_str = "2023-04-05" -- ISO 8601 formaat
local pattern = "(%d+)-(%d+)-(%d+)"
local jaar, maand, dag = date_str:match(pattern)
local datum_tabel = {year = jaar, month = maand, day = dag}

print(os.time(datum_tabel)) -- Voorbeeld output: 1679785200
```

En zo is je datum, geparsed en klaar!

## Diepere Duik
Lua is vrij minimalistisch, dus voor het parsen van datums moet je vaak je eigen oplossing verzinnen of een bibliotheek gebruiken. Historisch gezien was het omgaan met datums in Lua meestal handmatig, met string patroonherkenning en de `os.date` en `os.time` functies.

Als je het wiel niet opnieuw wilt uitvinden, bekijk dan bibliotheken zoals `Penlight` of `date.lua`. Deze bieden je meer flexibiliteit en kracht bij het omgaan met datums.

Wat implementatie betreft, onthoud dat Lua's patroonherkenning geen regex is; het is eenvoudiger en soms betekent dat iets meer werk om complexe datumformaten te parsen. Test je patronen altijd grondig!

## Zie Ook
- Lua 5.4 Referentiehandleiding voor `os.time` en patroonherkenning: https://www.lua.org/manual/5.4/
- Documentatie van de Penlight-bibliotheek: https://stevedonovan.github.io/Penlight/api/
- date.lua bibliotheek op GitHub voor een toegewijde datum-parsing oplossing: https://github.com/Tieske/date
