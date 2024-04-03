---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:08.177258-07:00
description: "Werken met CSV (Comma-Separated Values, ofwel door komma's gescheiden\
  \ waarden) betekent het parseren en genereren van tekstgegevens die door komma's\
  \ zijn\u2026"
lastmod: '2024-03-13T22:44:50.959388-06:00'
model: gpt-4-0125-preview
summary: Werken met CSV (Comma-Separated Values, ofwel door komma's gescheiden waarden)
  betekent het parseren en genereren van tekstgegevens die door komma's zijn begrensd.
title: Werken met CSV
weight: 37
---

## Hoe:
Laten we CSV-bestanden lezen en schrijven met Lua. We gaan een basisvoorbeeld behandelen zonder externe bibliotheken.

**Een CSV-bestand lezen:**

```Lua
function read_csv(filepath)
  local results = {}
  local file = assert(io.open(filepath, "r"))
  
  for line in file:lines() do
    table.insert(results, line:split(","))
  end
  
  file:close()
  return results
end

-- Een hulpfunctie om strings te splitsen
function string:split(delimiter)
  local result = {}
  local from = 1
  local delim_from, delim_to = self:find(delimiter, from, true)
  while delim_from do
    table.insert(result, self:sub(from, delim_from - 1))
    from = delim_to + 1
    delim_from, delim_to = self:find(delimiter, from, true)
  end
  table.insert(result, self:sub(from))
  return result
end
```

**Naar een CSV-bestand schrijven:**

```Lua
function write_csv(filepath, data)
  local file = assert(io.open(filepath, "w"))
  
  for _, row in ipairs(data) do
    file:write(table.concat(row, ",") .. "\n")
  end
  
  file:close()
end

-- Voorbeeldgegevens
local data = {
  { "Naam", "Leeftijd", "Stad" },
  { "Alice", "30", "New York" },
  { "Bob", "25", "Los Angeles" }
}

write_csv("output.csv", data)
```

## Diepere Duik
De geschiedenis van CSV gaat terug tot de vroege dagen van de informatica, waar eenvoud koning was. Terwijl JSON en XML nu rijkere gegevensstructuren bieden, blijft CSV populair vanwege de leesbaarheid en het gemak waarmee het kan worden bewerkt met spreadsheetsoftware. Let op bij de implementatie voor velden met komma's, nieuwe regels of aanhalingstekens - deze moeten correct worden geciteerd en/of geëscaped.

## Zie Ook
- De officiële Lua 5.4 referentiehandleiding: https://www.lua.org/manual/5.4/
- RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files: https://tools.ietf.org/html/rfc4180
- Penlight Lua Libraries (voor geavanceerdere CSV-behandeling): https://github.com/lunarmodules/Penlight
