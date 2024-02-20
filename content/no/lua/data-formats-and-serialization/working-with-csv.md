---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:38.430370-07:00
description: "\xC5 arbeide med CSV (kommaseparerte verdier) filer inneb\xE6rer parsing\
  \ og generering av tekstdata organisert i rader og kolonner, ved bruk av komma for\
  \ \xE5\u2026"
lastmod: 2024-02-19 22:05:00.218226
model: gpt-4-0125-preview
summary: "\xC5 arbeide med CSV (kommaseparerte verdier) filer inneb\xE6rer parsing\
  \ og generering av tekstdata organisert i rader og kolonner, ved bruk av komma for\
  \ \xE5\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å arbeide med CSV (kommaseparerte verdier) filer innebærer parsing og generering av tekstdata organisert i rader og kolonner, ved bruk av komma for å separere individuelle verdier. Programmerere engasjerer seg ofte i denne prosessen for å lette datautveksling mellom forskjellige applikasjoner, databaser, eller for oppgaver relatert til databehandling og analyse, på grunn av CSVs utbredte støtte og enkelhet.

## Hvordan:

I Lua kan man jobbe med CSV-filer ved å bruke grunnleggende fil I/O-operasjoner som språket tilbyr, uten behov for eksterne biblioteker for enkle oppgaver. For mer komplekse operasjoner, slik som håndtering av spesialtilfeller (f.eks., komma innenfor verdier), kan det være fordelaktig å bruke tredjepartsbiblioteker som `lua-csv`.

### Lese en CSV-fil
Her er et enkelt eksempel for å lese en CSV-fil linje for linje, splitte hver linje til verdier basert på kommaseparatoren.

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

**Eksempelutdata** (for en `example.csv` med innhold "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Skrive en CSV-fil
For å generere en CSV-fil, konstruerer du enkelt strenger med kommaseparerte verdier og skriver dem linje for linje til en fil.

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

Dette vil skape (eller overskrive) en `output.csv`-fil med den spesifiserte dataen.

### Bruke lua-csv
For mer avansert håndtering av CSV, inkludert støtte for anførselstegn og escape-tegn, er `lua-csv`-biblioteket et robust valg.

Først, installer det ved hjelp av LuaRocks:
```shell
luarocks install lua-csv
```

Deretter blir lesing av en CSV-fil så enkelt som:

```lua
local csv = require("csv")

-- Lese fra en fil
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

Og å skrive til en CSV med riktig quoting og escaping:

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

Denne tilnærmingen håndterer automatisk kompleksiteter som kommaer og anførselstegn innenfor verdier.
