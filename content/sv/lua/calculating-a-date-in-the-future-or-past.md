---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Lua: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift för programmerare. Det innebär helt enkelt att man tar ett befintligt datum och lägger till eller drar av ett antal dagar, veckor, månader eller år för att få en ny datum. Detta är användbart i många situationer, som att generera deadlines, almanackor eller planera resor.

## Hur man:

```Lua
-- Addera 50 dagar till dagens datum
local dagens_dag = os.date("*t")
dagens_dag.day = dagens_dag.day + 50
print(os.date("%Y-%m-%d", os.time(dagens_dag)))
-- Output: 2022-02-14

-- Subtrahera 2 veckor från ett specifikt datum
local datum = os.time{year=2021, month=10, day=10}
datum = datum - (14 * 24 * 60 * 60) -- 14 dagar i sekunder
print(os.date("%Y-%m-%d", datum))
-- Output: 2021-09-26
```

## Djupdykning:

Att beräkna datum i Lua är enkelt tack vare dess inbyggda funktioner för datum och tidsberäkning. Det finns också alternativ, som att använda en bibliotek som "date" eller "lua-date" för mer avancerade funktioner. Det är viktigt att ha koll på tidszoner, särskilt vid resor eller globala projekt, då olika regioner kan ha olika datum- och tidsformat.

## Se också:

- Lua funktionen för datum och tid: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
- Date biblioteket för Lua: https://keplerproject.github.io/date/index.html