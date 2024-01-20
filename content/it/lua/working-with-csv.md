---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Valori Separati da Virgole) è un formato di file usato per memorizzare dati strutturati. I programmatori lo utilizzano perché è semplice da leggere e scrivere sia da umani che da macchine.

## How to:
```Lua
-- Esempio di lettura di un file CSV
local file_path = "esempio.csv"

function parse_csv(file)
    local result = {}
    for line in io.lines(file) do
        local fields = {}
        for field in line:gmatch("[^,]+") do
            table.insert(fields, field)
        end
        table.insert(result, fields)
    end
    return result
end

local data = parse_csv(file_path)
print(data[1][1]) -- stampa il primo campo della prima riga
```

Output:
```
nome
```

```Lua
-- Esempio di scrittura in un file CSV
local data_to_write = {
    {"nome", "città", "età"},
    {"Mario", "Roma", 30},
    {"Luca", "Milano", 25}
}

local file_path = "salva_esempio.csv"

function write_csv(file, data)
    local file = io.open(file, "w")
    for _, row in ipairs(data) do
        file:write(table.concat(row, ",") .. "\n")
    end
    file:close()
end

write_csv(file_path, data_to_write)
```

Output:
Un file `salva_esempio.csv` con il contenuto:
```
nome,città,età
Mario,Roma,30
Luca,Milano,25
```

## Deep Dive
Il CSV ha origini agli albori dell'informatica; è diventato uno standard de facto per lo scambio di dati tabellari a causa della sua semplicità. Come alternative esistono JSON o XML, ma il CSV rimane popolare per file di piccole e medie dimensioni per la sua leggibilità e il basso overhead. Nella lettura del CSV, gestire virgolette e campi multi-linea può richiedere un parsing più sofisticato rispetto all'esempio mostrato.

## See Also
- Documentazione ufficiale di Lua: https://www.lua.org/manual/5.4/manual.html
- CSV su Wikipedia: https://it.wikipedia.org/wiki/Comma-separated_values
- Articolo su come gestire CSV con Lua più in dettaglio: https://lua-users.org/wiki/CsvUtils