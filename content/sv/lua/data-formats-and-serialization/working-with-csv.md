---
title:                "Arbeta med CSV"
aliases:
- sv/lua/working-with-csv.md
date:                  2024-02-03T19:20:47.990254-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV (Comma-Separated Values) filer innebär att tolka och generera textdata organiserad i rader och kolumner, där komman används för att separera enskilda värden. Programmerare engagerar sig ofta i denna process för att underlätta datautbyte mellan olika applikationer, databaser, eller för uppgifter avseende databearbetning och analys, på grund av CSV:s utbredda stöd och enkelhet.

## Hur man gör:

I Lua kan arbete med CSV-filer närmas genom grundläggande fil-IO-operationer som språket tillhandahåller, utan behov av externa bibliotek för enkla uppgifter. För mer komplexa operationer, såsom hantering av specialfall (t.ex., komman inom värden), kan det vara fördelaktigt att använda tredjepartsbibliotek som `lua-csv`.

### Läsa en CSV-fil
Här är ett enkelt exempel på att läsa en CSV-fil rad för rad, dela varje rad i värden baserat på kommatecknet som separator.

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**Exempelutskrift** (för en `example.csv` med innehållet "name,age\nJohn Doe,30\nJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Skriva en CSV-fil
För att generera en CSV-fil konstruerar du helt enkelt strängar med komma-separerade värden och skriver dem rad för rad till en fil.

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

Detta skulle skapa (eller skriva över) en `output.csv`-fil med de specificerade datan.

### Använda lua-csv
För mer avancerad hantering av CSV, inklusive stöd för citattecken och escape-tecken, är `lua-csv`-biblioteket ett robust val.

Först, installera det med LuaRocks:
```shell
luarocks install lua-csv
```

Sedan, att läsa en CSV-fil blir så enkelt som:

```lua
local csv = require("csv")

-- Läsa från en fil
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

Och skriva till en CSV med korrekt citering och escaping:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

Detta tillvägagångssätt hanterar automatiskt komplexiteter såsom komma och citattecken inom värden.
