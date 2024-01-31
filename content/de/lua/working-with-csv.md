---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV-Dateien speichern Tabellendaten in Klartext. Programmierer nutzen CSV, weil es einfach und weit verbreitet ist und sich leicht in Datenbanken oder Tabellenkalkulationen importieren lässt.

## How to:
CSV-Datei lesen:
```Lua
local filename = "beispieldaten.csv"

local function read_csv(filePath)
    local file = io.open(filePath, "r")
    local data = {}
    if not file then return nil, "Datei konnte nicht geöffnet werden" end

    for line in file:lines() do
        table.insert(data, line:split(","))
    end

    file:close()
    return data
end

-- Definiere zusätzlich die 'split' Funktion für Strings
function string:split(delimiter)
    local result = {}
    local from  = 1
    local delim_from, delim_to = string.find( self, delimiter, from  )
    while delim_from do
        table.insert( result, string.sub( self, from , delim_from-1 ) )
        from  = delim_to + 1
        delim_from, delim_to = string.find( self, delimiter, from  )
    end
    table.insert( result, string.sub( self, from  ) )
    return result
end

local data, error = read_csv(filename)
if not error then
    for i, row in ipairs(data) do
        for j, value in ipairs(row) do
            print(value)
        end
    end
else
    print(error)
end
```

CSV-Datei schreiben:
```Lua
local data = { {"Name", "Alter", "Stadt"}, {"Alice", "30", "Hamburg"}, {"Bob", "25", "Berlin"} }
local filename = "export.csv"

local function write_csv(data, filePath)
    local file = io.open(filePath, "w")
    if not file then return false, "Datei konnte nicht geschrieben werden" end

    for i, row in ipairs(data) do
        file:write(table.concat(row, ","), "\n")
    end

    file:close()
    return true
end

local success, error = write_csv(data, filename)
if success then
    print("CSV erfolgreich geschrieben.")
else
    print(error)
end
```

## Deep Dive
CSV steht für "Comma-separated values" und hat seit den frühen Computerzeiten an Bedeutung gewonnen. Alternativen wie JSON oder XML bieten strukturierte Datenformate, sind aber komplexer im Handling. Die Implementierung von CSV in Lua erfordert den Umgang mit Datei-IO und String-Manipulation, da Lua keine eingebaute CSV-Unterstützung hat.

## See Also
- Lua-Referenzhandbuch: https://www.lua.org/manual/5.4/
- Tutorial für Dateioperationen in Lua: https://www.tutorialspoint.com/lua/lua_file_io.htm
- CSV zu Lua Table Konverter online: https://www.convertcsv.com/csv-to-lua.htm
