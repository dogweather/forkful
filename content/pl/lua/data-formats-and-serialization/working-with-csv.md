---
title:                "Praca z plikami CSV"
aliases: - /pl/lua/working-with-csv.md
date:                  2024-02-03T19:20:47.757202-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV (Comma-Separated Values), czyli wartościami oddzielonymi przecinkami, polega na parsowaniu i generowaniu danych tekstowych zorganizowanych w wiersze i kolumny, przy użyciu przecinków do oddzielania poszczególnych wartości. Programiści często angażują się w ten proces, aby ułatwić wymianę danych między różnymi aplikacjami, bazami danych lub do zadań przetwarzania i analizy danych, ze względu na powszechne wsparcie i prostotę CSV.

## Jak to zrobić:

W Lua, praca z plikami CSV może być podejmowana za pomocą podstawowych operacji wejścia/wyjścia plików oferowanych przez język, bez potrzeby używania zewnętrznych bibliotek do prostych zadań. Dla bardziej skomplikowanych operacji, takich jak obsługa specjalnych przypadków (np. przecinki w wartościach), korzystanie z bibliotek stron trzecich, takich jak `lua-csv`, może być korzystne.

### Odczytywanie pliku CSV
Oto prosty przykład odczytywania pliku CSV linia po linii, dzieląc każdą linię na wartości na podstawie separatora przecinka.

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

**Przykładowe wyjście** (dla `example.csv` z treścią "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Tworzenie pliku CSV
Aby wygenerować plik CSV, po prostu konstruuje się ciągi z wartościami oddzielonymi przecinkami i zapisuje się je do pliku linia po linii.

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

To spowoduje utworzenie (lub nadpisanie) pliku `output.csv` z określonymi danymi.

### Użycie lua-csv
Do bardziej zaawansowanego obsługiwania plików CSV, w tym wsparcia dla cudzysłowów i znaków ucieczki, biblioteka `lua-csv` jest solidnym wyborem.

Najpierw zainstaluj ją za pomocą LuaRocks:
```shell
luarocks install lua-csv
```

Następnie, odczytywanie pliku CSV staje się tak proste jak:

```lua
local csv = require("csv")

-- Odczytywanie z pliku
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

I zapisywanie do CSV z odpowiednim cytowaniem i ucieczką:

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

To podejście automatycznie radzi sobie ze skomplikowanymi problemami, takimi jak przecinki i cudzysłowy w wartościach.
