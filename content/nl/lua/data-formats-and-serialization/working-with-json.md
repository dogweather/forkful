---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:35.324505-07:00
description: "JSON (JavaScript Object Notation) wordt gebruikt om gegevens op te slaan\
  \ en te transporteren. Programmeurs gebruiken JSON omdat het lichtgewicht is,\u2026"
lastmod: '2024-03-13T22:44:50.958450-06:00'
model: gpt-4-0125-preview
summary: JSON (JavaScript Object Notation) wordt gebruikt om gegevens op te slaan
  en te transporteren.
title: Werken met JSON
weight: 38
---

## Hoe:
Laten we wat JSON parsen.

```lua
-- Zorg dat je de 'dkjson' module of een andere JSON-bibliotheek hebt.
local dkjson = require 'dkjson'

local jsonString = '{"name":"John", "age":30, "city":"New York"}'

-- Parse JSON-string naar een Lua-tabel.
local person, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
    print("Fout:", err)
else
    print(person.name)  -- Uitvoer: John
end

-- Converteer Lua-tabel naar JSON-string.
local personTable = { name = "Jane", age = 25, city = "Los Angeles" }
local jsonOutput = dkjson.encode(personTable)
print(jsonOutput)  -- Uitvoer: {"age":25,"city":"Los Angeles","name":"Jane"}
```

Nu laten we arrays afhandelen.

```lua
local jsonArrayString = '[{"name":"John"}, {"name":"Jane"}]'

-- Parse JSON-string met een array naar een Lua-tabel.
local peopleArray, _, err = dkjson.decode(jsonArrayString)
if err then
    print("Fout:", err)
else
    for i, person in ipairs(peopleArray) do
        print(person.name)  -- Uitvoer: John\nJane
    end
end
```

## Diepgaand
JSON werd de de facto standaard voor API's, en groeide uit boven XML omdat het minder verhalend is. Er zijn alternatieven zoals YAML, dat nog leesbaarder is maar niet zo breed gebruikt wordt in API's. In Lua is er geen native JSON-ondersteuning, dus je hebt een bibliotheek zoals 'dkjson' of 'cjson' nodig. Implementatiedetails in Lua omvatten het omgaan met typeverschillen, zoals arrays en objecten, en het converteren tussen Lua's `nil` en JSON's `null`.

## Zie Ook
- [dkjson-bibliotheek op GitHub](https://github.com/LuaDist/dkjson)
- [Officiële JSON-website](https://www.json.org/json-en.html)
- [Programmeren in Lua (eerste editie)](https://www.lua.org/pil/contents.html) voor het leren van de basis van Lua.
