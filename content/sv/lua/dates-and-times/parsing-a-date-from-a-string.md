---
title:                "Analysera ett datum från en sträng"
aliases: - /sv/lua/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:01.636083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera ett datum från en sträng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att konvertera textuella representationer av datum och tider till ett format som kan hanteras, lagras eller jämföras enkelt inom ett Lua-program. Programmerare utför denna uppgift för att underlätta operationer såsom schemaläggning, loggning eller alla typer av tidsberäkningar och för att överbrygga klyftan mellan datumformat som är läsbara för människor och strukturerade datatyper som en dator kan bearbeta effektivt.

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
