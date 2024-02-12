---
title:                "Analysering av en dato fra en streng"
aliases:
- /no/lua/parsing-a-date-from-a-string/
date:                  2024-02-03T19:15:05.833430-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av en dato fra en streng innebærer å konvertere tekstuelle representasjoner av datoer og tider til et format som kan enkelt manipuleres, lagres eller sammenlignes innenfor et Lua-program. Programmerere utfører denne oppgaven for å lette operasjoner som planlegging, logging eller alle tidsberegninger og for å bygge bro mellom menneskelesbare datoformater og strukturerte datatyper som en datamaskin kan behandle effektivt.

## Hvordan:
Lua har ikke innebygget støtte for dato- og tidsmanipulering utover den begrensede funksjonaliteten som tilbys av `os.date` og `os.time`-funksjonene. Imidlertid kan disse benyttes for grunnleggende parsing, og for mer komplekse krav, kan `luadate`-biblioteket, et eksternt bibliotek, benyttes.

**Bruk av `os.date` og `os.time`:**
```lua
-- Konverter en menneskelesbar dato til et tidsstempel og tilbake
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local år, måned, dag, time, minutt, sekund = dateString:match(pattern)

local tidsstempel = os.time({
  year = år,
  month = måned,
  day = dag,
  hour = time,
  min = minutt,
  sec = sekund
})

-- Konverter tidsstempel tilbake til et menneskelesbart format
local formatertDato = os.date("%Y-%m-%d %H:%M:%S", tidsstempel)
print(formatertDato)  -- Utdata: 2023-09-21 15:00:00
```

**Bruk av `luadate` (tredjepartsbibliotek):**
For å bruke `luadate`, sørg for at det er installert via LuaRocks eller pakkebehandleren du foretrekker. `luadate` tilfører omfattende parsing og manipuleringsmuligheter for dato og tid.

```lua
local date = require('date')

-- Parse en datostreng direkte
local parsetDato = date.parse("2023-09-21 15:00:00")
print(parsetDato:fmt("%Y-%m-%d %H:%M:%S"))  -- Utdata: 2023-09-21 15:00:00

-- Legge til varigheter
local enUkeSenere = parsetDato:adddays(7)
print(enUkeSenere:fmt("%Y-%m-%d %H:%M:%S"))  -- Utdata: 2023-09-28 15:00:00
```

`luadate`-biblioteket tilbyr en mer intuitiv og kraftfull måte å jobbe med datoer på, inkludert parsing fra strenger, formatering, og aritmetiske operasjoner på datoer, noe som betydelig forenkler arbeid med tidsdata i Lua.
