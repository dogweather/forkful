---
date: 2024-01-20 17:46:24.114316-07:00
description: "Hur g\xF6r man?: I Lua h\xE4mtas delstr\xE4ngar genom `string.sub`-funktionen,\
  \ vilken har sitt ursprung i ANSI C's `substr`-funktion. Andra programmeringsspr\xE5\
  k\u2026"
lastmod: '2024-04-05T21:53:39.367754-06:00'
model: gpt-4-1106-preview
summary: "I Lua h\xE4mtas delstr\xE4ngar genom `string.sub`-funktionen, vilken har\
  \ sitt ursprung i ANSI C's `substr`-funktion."
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## Hur gör man?:
```Lua
local str = "Hej, hur mår du idag?"
local delstrang = str:sub(6, 8)  -- Extraherar 'hur'
print(delstrang)  -- Output: hur

local annanDelstrang = str:sub(1, 3)  -- Extraherar 'Hej'
print(annanDelstrang)  -- Output: Hej
```

## Fördjupning:
I Lua hämtas delsträngar genom `string.sub`-funktionen, vilken har sitt ursprung i ANSI C's `substr`-funktion. Andra programmeringsspråk använder liknande funktioner men med olika syntax. Alternativ till `string.sub` i Lua är funktioner som `string.match` för specifika mönster eller `string.gmatch` för iteration över flera träffar. Kännedom om vilken metod som är effektivast för specifika fall är viktigt för prestandan, eftersom strängmanipulering kan vara resurskrävande.

## Se Även:
- Lua 5.4 referenshandbok: https://www.lua.org/manual/5.4/
- 'Programming in Lua' (första utgåvan gratis online): https://www.lua.org/pil/
