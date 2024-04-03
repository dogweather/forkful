---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:01.636083-07:00
description: "Hur: Lua har inte inbyggt st\xF6d f\xF6r hantering av datum och tid\
  \ ut\xF6ver den begr\xE4nsade funktionaliteten som tillhandah\xE5lls av funktionerna\
  \ `os.date`och\u2026"
lastmod: '2024-03-13T22:44:38.047873-06:00'
model: gpt-4-0125-preview
summary: "Lua har inte inbyggt st\xF6d f\xF6r hantering av datum och tid ut\xF6ver\
  \ den begr\xE4nsade funktionaliteten som tillhandah\xE5lls av funktionerna `os.date`och\
  \ `os.time`."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur:
Lua har inte inbyggt stöd för hantering av datum och tid utöver den begränsade funktionaliteten som tillhandahålls av funktionerna `os.date`och `os.time`. Dessa kan dock utnyttjas för grundläggande tolkning, och för mer komplexa krav kan `luadate`-biblioteket, ett externt bibliotek, användas.

**Användning av `os.date` och `os.time`:**
```lua
-- Konvertera ett läsbart datum till en tidsstämpel och tillbaka
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local år, månad, dag, timme, minut, sekund = dateString:match(pattern)

local tidsstämpel = os.time({
  år = år,
  månad = månad,
  dag = dag,
  timme = timme,
  min = minut,
  sek = sekund
})

-- Konvertera tidsstämpel tillbaka till ett format läsbart för människor
local formateratDatum = os.date("%Y-%m-%d %H:%M:%S", tidsstämpel)
print(formateratDatum)  -- Utdata: 2023-09-21 15:00:00
```

**Användning av `luadate` (tredjepartsbibliotek):**
För att använda `luadate`, se till att det är installerat via LuaRocks eller din valda pakethanterare. `luadate` lägger till omfattande funktioner för tolkning och hantering av datum och tid.

```lua
local date = require('date')

-- Tolkning av en datumsträng direkt
local tolkatDatum = date.parse("2023-09-21 15:00:00")
print(tolkatDatum:fmt("%Y-%m-%d %H:%M:%S"))  -- Utdata: 2023-09-21 15:00:00

-- Lägga till varaktigheter
local enVeckaSenare = tolkatDatum:adddays(7)
print(enVeckaSenare:fmt("%Y-%m-%d %H:%M:%S"))  -- Utdata: 2023-09-28 15:00:00
```

`luadate`-biblioteket erbjuder ett mer intuitivt och kraftfullt sätt att arbeta med datum, inklusive tolkning från strängar, formatering och aritmetiska operationer på datum, vilket avsevärt förenklar hanteringen av tidsdata i Lua.
