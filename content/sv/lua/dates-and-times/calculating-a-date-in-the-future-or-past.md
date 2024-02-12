---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
aliases: - /sv/lua/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:28.802374-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkna ett datum i framtiden eller förflutet innebär att man ändrar ett specifikt datum med ett visst antal dagar, månader eller år. Programmerare gör det för att hantera deadlines, händelseplanering, eller att spåra tidsskillnader.

## Så här gör du:
I Lua använder du `os.date` för läsbarhet och `os.time` för aritmetik med tidsstämplar. Här är några enkla exempel:

```lua
-- Nuvarande datum och tid
local now = os.time()

-- Räkna ut datumet för 7 dagar framåt
local seven_days = os.time({year=os.date("%Y", now), month=os.date("%m", now), day=os.date("%d", now) + 7})
print(os.date("%Y-%m-%d", seven_days))  -- Exempel på utskrift: "2023-03-30"

-- Räkna ut datumet för 30 dagar sedan
local thirty_days_ago = os.time({year=os.date("%Y", now), month=os.date("%m", now), day=os.date("%d", now) - 30})
print(os.date("%Y-%m-%d", thirty_days_ago))  -- Exempel på utskrift: "2023-02-28"
```

Observera att Lua inte hanterar övergången mellan månader eller år automatiskt, så du måste hantera dessa scenarion själv.

## Djupdykning
I Lua är `os.date` och `os.time` standardfunktionerna för datum och tid. Historiskt sett har datumhantering varit en källa till buggar på grund av dess komplexitet, såsom skottår och olika tidszoner. Alternativ till den inbyggda funktionaliteten inkluderar externa bibliotek som `luadate`, vilket kan hantera mer komplexa datumoperationer och tidszoner. När du beräknar datum i framtiden eller förflutet, kom ihåg att kontrollera kantfall som skottdagar och klockan som ställs om för sommar- och vintertid.

## Se även
- Lua 5.4 Referensmanual: https://www.lua.org/manual/5.4/manual.html#6.9
- GitHub-repo för luadate, ett datumhanteringsbibliotek för Lua: https://github.com/Tieske/date
- Information om tid och datum i programmering allmänt: https://en.wikipedia.org/wiki/System_time
