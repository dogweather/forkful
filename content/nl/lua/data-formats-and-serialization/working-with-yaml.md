---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:52.310338-07:00
description: "YAML is een gegevensserialisatieformaat dat gemakkelijk door mensen\
  \ te lezen en te schrijven is. Programmeurs gebruiken het voor configuratiebestanden,\u2026"
lastmod: '2024-03-13T22:44:50.957527-06:00'
model: gpt-4-0125-preview
summary: YAML is een gegevensserialisatieformaat dat gemakkelijk door mensen te lezen
  en te schrijven is.
title: Werken met YAML
weight: 41
---

## Hoe te:
Lua heeft geen ingebouwde ondersteuning voor YAML, maar je kunt een bibliotheek zoals `lyaml` gebruiken. Installeer het met `luarocks install lyaml`. Hier is hoe je YAML kunt parsen:

```Lua
local lyaml = require('lyaml')

-- Voorbeeld YAML-gegevens als een string
local yaml_data = [[
- name: John Doe
  age: 29
- name: Jane Smith
  age: 42
]]

-- YAML-string parsen naar Lua-tabel
local parsed_data = lyaml.load(yaml_data)

-- Gegevens benaderen
for i, person in ipairs(parsed_data) do
  print(person.name, person.age)
end
```

Voorbeelduitvoer:
```
John Doe 29
Jane Smith 42
```

Laten we nu wat YAML genereren vanuit een Lua-tabel:

```Lua
local lyaml = require('lyaml')

-- Voorbeeld Lua-tabel
local people = {
  { name = "John Doe", age = 29 },
  { name = "Jane Smith", age = 42 }
}

-- YAML genereren vanuit Lua-tabel
local yaml_output = lyaml.dump(people)

print(yaml_output)
```

Voorbeeld YAML-uitvoer:
```
- age: 29
  name: John Doe
- age: 42
  name: Jane Smith
```

## Diepgaand
YAML, wat staat voor "YAML Ain't Markup Language", kwam begin jaren 2000 op als een gebruiksvriendelijke standaard voor gegevensserialisatie. Het is minder uitgebreid dan XML en JSON, wat het populair maakt voor configuratiebestanden. Alternatieven zijn JSON, XML en TOML. Lua-implementatie is voornamelijk afhankelijk van externe bibliotheken zoals `lyaml`, die libYAML gebruikt voor het parsen en uitvoeren van YAML. Wanneer je YAML met Lua gebruikt, onthoud dan dat tabellen geen inherente volgorde hebben, dus lijsten in YAML worden arrays, maar woordenboeken (sleutel-waardeparen) behouden mogelijk niet hun volgorde.

## Zie Ook
- Officiële website van YAML: https://yaml.org
- `lyaml` bibliotheek op GitHub: https://github.com/gvvaughan/lyaml
- LuaRocks-pakket voor `lyaml`: https://luarocks.org/modules/gvvaughan/lyaml
