---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:35.324505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

JSON (JavaScript Object Notation) wordt gebruikt om gegevens op te slaan en te transporteren. Programmeurs gebruiken JSON omdat het lichtgewicht is, gemakkelijk voor mensen om te lezen en te schrijven, en gemakkelijk voor machines om te parsen en te genereren.

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
