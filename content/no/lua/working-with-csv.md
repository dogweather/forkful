---
title:                "Arbeid med CSV"
date:                  2024-01-19
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) brukes for å lagre data i enkel tekstformat. Det er nyttig fordi det enkelt kan leses av mennesker og maskiner og støttes av mange programmeringsspråk og applikasjoner.

## How to:
Her er enkle steg for å lese og skrive CSV filer i Lua.

```Lua
-- Skrive til CSV
local data = {
    {"navn", "alder", "by"},
    {"Ola", 28, "Oslo"},
    {"Kari", 35, "Bergen"}
}

local file = io.open("eksempel.csv", "w")
for i, row in ipairs(data) do
    file:write(table.concat(row, ","), "\n")
end
file:close()

-- Lese fra CSV
local function read_csv(file_path)
    local file = io.open(file_path, "r")
    local data = {}
    for line in file:lines() do
        table.insert(data, line:split(","))
    end
    file:close()
    return data
end

-- Anta at `string.split` funksjonen er definert
local data = read_csv("eksempel.csv")
for i, row in ipairs(data) do
    print(table.concat(row, ", "))
end
```
Dette ville ha gitt følgende i konsollen for lese-delen:
```
navn, alder, by
Ola, 28, Oslo
Kari, 35, Bergen
```

## Deep Dive
CSV er ikke nytt, det stammer fra 1972. Mens enkelhet er styrken, mangler det standardisering; forskjeller i escaping, deltegn og tegnsett kan skape hodebry. Alternativer som JSON eller XML gir mer struktur og datakompleksitet håndterbar, men de er ikke alltid like menneskelesbare. Lua håndterer ikke CSV naturlig, så en brukerdefinert funksjon eller et eksternt bibliotek kreves.

## See Also
- [Lua.org](https://www.lua.org) – Offisielt nettsted for Lua.
- [Programming in Lua](https://www.lua.org/pil/contents.html) – En detaljert bok om hvordan du programmerer i Lua.
