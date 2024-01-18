---
title:                "Att avläsa ett datum från en sträng"
html_title:           "Lua: Att avläsa ett datum från en sträng"
simple_title:         "Att avläsa ett datum från en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "parsa" ett datum från en sträng är en process där man extraherar ett specifikt datum från en lång sträng av text. Detta kan vara användbart för programmerare som behöver konvertera datum från ett format till ett annat, eller för att utföra olika beräkningar baserat på ett specifikt datum.

## Så här gör du:
```Lua 
-- Konvertera en sträng till ett datumobjekt
local dateString = "2020-07-31" -- En sträng som innehåller datumet
local date = os.date("%Y-%m-%d", dateString) -- Parsa strängen med hjälp av os.date()
print(date) -- Skriver ut "2020-07-31" som resultat

-- Beräkna antalet dagar mellan två datum
local start = os.time({year = 2020, month = 7, day = 30}) -- Skapa ett datumobjekt för startdatumet
local finish = os.time({year = 2020, month = 8, day = 5}) -- Skapa ett datumobjekt för slutdatumet
local daysBetween = (finish - start) / (60 * 60 * 24) -- Beräkna antal sekunder, sedan omvandla till dagar
print(daysBetween) -- Skriver ut "6" som resultat
 ```
 
## Djupdykning:
Att kunna parsa datum från strängar har varit en viktig del av programmering sedan det första programmeringsspråket, FORTRAN, introducerades 1957. Det finns också alternativ till att använda os.date() för att utföra denna uppgift, till exempel med hjälp av formatmallar i bibliotek som "luatz" eller "LuaPowerDate". Implementationen av parsning av datum från en sträng skiljer sig också åt mellan språk och kan vara mer eller mindre komplicerad beroende på programmets behov.

## Se även:
För mer information om Lua:s os.date() och hur man kan använda det för att parsa datum från strängar, kolla in dessa källor:
- [Officiell Lua-dokumentation](https://www.lua.org/manual/5.3/manual.html#pdf-os.date)
- [LuaZ-dokumentation](https://download.lallafa.de/DOCS/LuaFun/LuaZ.html#ZDate)
- [LuaPowerDate-dokumentation](https://bitbucket.org/LuaPowerDev/lua-power-date/wiki/Home)