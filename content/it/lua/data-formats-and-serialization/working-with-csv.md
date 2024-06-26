---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:29.440636-07:00
description: "Come fare: In Lua, lavorare con file CSV pu\xF2 essere affrontato utilizzando\
  \ le operazioni di IO su file di base fornite dal linguaggio, senza la necessit\xE0\
  \u2026"
lastmod: '2024-03-13T22:44:43.579355-06:00'
model: gpt-4-0125-preview
summary: "In Lua, lavorare con file CSV pu\xF2 essere affrontato utilizzando le operazioni\
  \ di IO su file di base fornite dal linguaggio, senza la necessit\xE0 di librerie\
  \ esterne per compiti semplici."
title: Lavorare con i CSV
weight: 37
---

## Come fare:
In Lua, lavorare con file CSV può essere affrontato utilizzando le operazioni di IO su file di base fornite dal linguaggio, senza la necessità di librerie esterne per compiti semplici. Per operazioni più complesse, come la gestione di casi speciali (ad esempio, virgole all'interno dei valori), potrebbe essere vantaggioso utilizzare librerie di terze parti come `lua-csv`.

### Leggere un file CSV
Ecco un esempio semplice per leggere un file CSV riga per riga, dividendo ogni riga in valori basati sul separatore di virgola.

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

**Esempio di output** (per un `example.csv` con contenuto "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Scrivere un file CSV
Per generare un file CSV, è sufficiente costruire stringhe con valori separati da virgole e scriverle in un file riga per riga.

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

Questo creerebbe (o sovrascriverebbe) un file `output.csv` con i dati specificati.

### Usare lua-csv
Per una gestione CSV più avanzata, inclusa il supporto per virgolette e caratteri di escape, la libreria `lua-csv` è una scelta robusta.

Prima, installala usando LuaRocks:
```shell
luarocks install lua-csv
```

Poi, leggere un file CSV diventa semplice come:

```lua
local csv = require("csv")

-- Lettura da un file
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

E scrivere su un CSV con virgolettatura ed escaping appropriati:

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

Questo approccio gestisce automaticamente complessità come virgole e virgolette all'interno dei valori.
