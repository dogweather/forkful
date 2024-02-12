---
title:                "Een datum in de toekomst of het verleden berekenen"
aliases: - /nl/lua/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:25.541405-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het berekenen van toekomstige of verleden data betekent uitzoeken wat de datum zal zijn na of voor een bepaalde hoeveelheid tijd. Programmeurs doen dit voor functies zoals herinneringen, abonnementen, of om voorbije gebeurtenissen bij te houden.

## Hoe te:

In Lua heb je de functies `os.date` en `os.time` tot je beschikking om te helpen met berekeningen van datum en tijd.

```Lua
-- Dagen toevoegen aan de huidige datum
local dagenToevoegen = 10
local toekomstigeDatum = os.time() + (dagenToevoegen * 24 * 60 * 60) -- dagen * uren * minuten * seconden
print("Toekomstige Datum: " .. os.date("%Y-%m-%d", toekomstigeDatum))

-- Dagen aftrekken van de huidige datum
local dagenAftrekken = 5
local verledenDatum = os.time() - (dagenAftrekken * 24 * 60 * 60) -- zelfde omzetting als hierboven
print("Verleden Datum: " .. os.date("%Y-%m-%d", verledenDatum))
```

Een voorbeeld van de output zou kunnen zijn:
```
Toekomstige Datum: 2023-05-03
Verleden Datum: 2023-04-18
```

## Diepgaande Duik

Lua's `os.date` en `os.time` functies zijn geworteld in de standaard C bibliotheek. Dit betekent dat ze dicht bij de hardware staan — efficiënt en betrouwbaar. Ze houden zich niet bezig met zaken zoals tijdzones of zomertijd; ze gaan om met UTC en seconden sinds het Unix-tijdperk (1 januari 1970).

Alternatieven voor `os.date` en `os.time` bestaan als je naar meer zoekt. Bibliotheken zoals `Luadate` bieden meer geavanceerde bewerkingen, die tijdzones en zomertijd met meer finesse behandelen.

Als het op implementatie aankomt, houd rekening met schrikkelseconden, en onthoud dat een maand toevoegen niet zo simpel is als 30 dagen toevoegen. Verschillende maanden hebben een verschillend aantal dagen, en februari kan je ofwel tekortdoen of verrassen met een extra dag.

## Zie Ook

Voor een luxere datum- en tijdervaring in Lua, bekijk deze bronnen:

- LuaRocks `Luadate`: https://luarocks.org/modules/luarocks/luadate
- Lua-gebruikers wiki over datum en tijd: http://lua-users.org/wiki/DateTime
- De `os` bibliotheekreferentie in de Lua 5.4 handleiding: https://www.lua.org/manual/5.4/manual.html#6.9
