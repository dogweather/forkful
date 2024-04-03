---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:34.389717-07:00
description: "Wie geht das: In Lua kann die Arbeit mit CSV-Dateien durch grundlegende\
  \ Datei-IO-Operationen, die von der Sprache bereitgestellt werden, ohne die\u2026"
lastmod: '2024-03-13T22:44:54.037702-06:00'
model: gpt-4-0125-preview
summary: "In Lua kann die Arbeit mit CSV-Dateien durch grundlegende Datei-IO-Operationen,\
  \ die von der Sprache bereitgestellt werden, ohne die Notwendigkeit externer Bibliotheken\
  \ f\xFCr einfache Aufgaben, angegangen werden."
title: Arbeiten mit CSV
weight: 37
---

## Wie geht das:
In Lua kann die Arbeit mit CSV-Dateien durch grundlegende Datei-IO-Operationen, die von der Sprache bereitgestellt werden, ohne die Notwendigkeit externer Bibliotheken für einfache Aufgaben, angegangen werden. Für komplexere Operationen, wie z.B. die Behandlung von Sonderfällen (z.B. Kommas innerhalb von Werten), könnte es vorteilhaft sein, Drittanbieter-Bibliotheken wie `lua-csv` zu verwenden.

### Eine CSV-Datei lesen
Hier ist ein einfaches Beispiel, um eine CSV-Datei Zeile für Zeile zu lesen, wobei jede Zeile basierend auf dem Kommatrennzeichen in Werte aufgeteilt wird.

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

**Beispielausgabe** (für eine `example.csv` mit Inhalt "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Eine CSV-Datei schreiben
Um eine CSV-Datei zu generieren, konstruiert man einfach Zeichenketten mit kommagetrennten Werten und schreibt sie Zeile für Zeile in eine Datei.

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

Dies würde eine `output.csv`-Datei mit den angegebenen Daten erstellen (oder überschreiben).

### lua-csv verwenden
Für fortgeschrittenere CSV-Handhabungen, einschließlich Unterstützung für Anführungszeichen und Escape-Zeichen, ist die Bibliothek `lua-csv` eine robuste Wahl.

Zuerst installieren Sie sie mit LuaRocks:
```shell
luarocks install lua-csv
```

Dann wird das Lesen einer CSV-Datei so einfach wie:

```lua
local csv = require("csv")

-- Lesen aus einer Datei
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

Und das Schreiben in eine CSV mit korrekter Quotierung und Escaping:

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

Dieser Ansatz behandelt automatisch Komplexitäten wie Kommas und Anführungszeichen innerhalb von Werten.
