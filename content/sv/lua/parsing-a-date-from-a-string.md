---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:37:31.715968-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka (parse) ett datum från en sträng innebär att omvandla text till ett datumformat som programmet kan hantera. Programmerare gör detta för att enkelt kunna bearbeta och manipulera datum som användaren anger eller som finns i datakällor.

## Hur gör man:
```Lua
-- Exempel för att tolka ett datum från en sträng
os.setlocale('sv_SE')  -- Ställ in lokala inställningar till svenska

local input_str = "2023-04-12"
local pattern = "(%d+)-(%d+)-(%d+)"
local year, month, day = input_str:match(pattern)

-- Omvandlar strängdelarna till nummer
year, month, day = tonumber(year), tonumber(month), tonumber(day)

-- Skapa ett os.date tabell
local date_table = {year = year, month = month, day = day}

-- Formatera och visa datumet
print(os.date("%Y-%m-%d", os.time(date_table)))
```
Sample Output:
```
2023-04-12
```

## Fördjupning
Historiskt sett har datum och tidsbearbetning alltid varit viktiga i programmering för att hantera händelser och loggning. Lua hanterar datum via `os.date` och `os.time` funktioner. Alternativ till inbyggda metoder inkluderar att använda externa bibliotek som `LuaDate` för mer avancerade behov. En viktig detalj vid datumtolkning är att alltid vara medveten om tidszoner och hur de påverkar datum och tid.

## Se även
- Lua 5.4 reference manual om `os.date` och `os.time`: https://www.lua.org/manual/5.4/manual.html#6.9
- LuaDate, ett datum- och tidshantering bibliotek för Lua: https://github.com/Tieske/date
- Lua användarguide för datum och tid: http://lua-users.org/wiki/DateTimeFunctions