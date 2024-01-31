---
title:                "Arbeta med csv"
date:                  2024-01-19
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV står för "Comma-Separated Values" och används för att lagra data på ett tabelliknande, textbaserat format. Programmerare använder det för dess enkelhet att importera, exportera och bearbeta data mellan olika system och applikationer.

## Steg för steg:
För att läsa och skriva CSV-filer i Lua, kan vi använda den inbyggda `io`-biblioteket. Där kan vi enkelt iterera genom varje rad och dela upp raderna baserat på kommatecken.

```Lua
-- Läs in en CSV-fil
local function read_csv(filepath)
    local result = {}
    local file = io.open(filepath, "r")

    for line in file:lines() do
        table.insert(result, line:split(","))
    end

    file:close()
    return result
end

-- Skriv ut till en CSV-fil
local function write_csv(filepath, data)
    local file = io.open(filepath, "w+")
    
    for _, row in ipairs(data) do
        file:write(table.concat(row, ",") .. "\n")
    end

    file:close()
end
```
Antag att vi har en CSV-fil `data.csv` med följande innehåll:
```
name,age,city
Alice,30,Stockholm
Bob,25,Göteborg
```
Exemplet ovan skulle läsa filen och skriva datan till en ny CSV-fil.

## Fördjupning
CSV-formatet har använts sedan 1970-talet och är ett av de enklaste sätten att importera och exportera enkelt strukturerad data. Lua saknar native stöd för CSV men dess flexibilitet låter en lätt implementera funktionalitet med basbiblioteket `io`. Alternativ till CSV inkluderar JSON och XML, vilka hanterar komplexa datastrukturer bättre men är inte lika raka att arbeta med för enkla datatabeller.

## Se även
- Lua Manual för IO-biblioteket: https://www.lua.org/manual/5.4/manual.html#6.8
- CSV på Wikipedia: https://sv.wikipedia.org/wiki/Komma-separerade_v%C3%A4rden
- Lua CSV moduler på LuaRocks: https://luarocks.org/search?q=csv
