---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:21.943436-07:00
description: "Hur man g\xF6r: Lua tillhandah\xE5ller funktionen `os.date` f\xF6r att\
  \ f\xE5 det aktuella datumet och tiden. Funktionen kan anv\xE4ndas utan argument\
  \ f\xF6r att f\xE5 en\u2026"
lastmod: '2024-03-13T22:44:38.049120-06:00'
model: gpt-4-0125-preview
summary: "Lua tillhandah\xE5ller funktionen `os.date` f\xF6r att f\xE5 det aktuella\
  \ datumet och tiden."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur man gör:
Lua tillhandahåller funktionen `os.date` för att få det aktuella datumet och tiden. Funktionen kan användas utan argument för att få en formaterad sträng eller med formatspecifierare för att anpassa utdatat. Så här använder du den:

```lua
-- Få det aktuella datumet och tiden som en formaterad sträng
print(os.date())  -- t.ex., Thu Mar  3 14:02:03 2022

-- Anpassa utdataformatet
-- %Y för år, %m för månad, %d för dag, %H för timme, %M för minuter
print(os.date("%Y-%m-%d %H:%M"))  -- t.ex., 2022-03-03 14:02
```

För mer avancerad manipulation av datum och tid har Lua inte inbyggda bibliotek så rika som vissa andra programmeringsspråk. Du kan dock använda tredjepartsbibliotek såsom `lua-date` (https://github.com/Tieske/date). Detta bibliotek erbjuder mer omfattande funktionaliteter för att manipulera datum och tider. Så här kan du använda det:

Först, se till att du har installerat `lua-date`-biblioteket. Du kan vanligtvis installera det med LuaRocks med följande kommando:

```bash
luarocks install lua-date
```

Sedan kan du använda det i ditt Lua-skript så här:

```lua
local date = require("date")

-- Skapa ett datumobjekt för det aktuella datumet och tiden
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- t.ex., 2022-03-03 14:02:03
```

Detta exempel demonstrerar skapandet av ett `date`-objekt som representerar det aktuella ögonblicket, vilket du sedan kan formatera på ett liknande sätt till funktionen `os.date` men med tillagd flexibilitet och alternativ som tillhandahålls av `lua-date`-biblioteket.
