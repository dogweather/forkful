---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:23.127090-07:00
description: "Comment faire : Lua n'inclut pas de biblioth\xE8que int\xE9gr\xE9e pour\
  \ le traitement de JSON. Par cons\xE9quent, l'une des biblioth\xE8ques tierces populaires\
  \ est\u2026"
lastmod: '2024-03-13T22:44:57.961753-06:00'
model: gpt-4-0125-preview
summary: "Lua n'inclut pas de biblioth\xE8que int\xE9gr\xE9e pour le traitement de\
  \ JSON."
title: Travailler avec JSON
weight: 38
---

## Comment faire :
Lua n'inclut pas de bibliothèque intégrée pour le traitement de JSON. Par conséquent, l'une des bibliothèques tierces populaires est `dkjson`, que vous pouvez facilement utiliser pour l'encodage et le décodage JSON. Premièrement, assurez-vous d'installer `dkjson`, par exemple, via LuaRocks (`luarocks install dkjson`), puis suivez les exemples ci-dessous.

### Décodage de JSON en Table Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Programmeur Lua", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Erreur:", err)
else
  print("Nom:", luaTable.name) -- Sortie: Nom: Programmeur Lua
  print("Âge:", luaTable.age) -- Sortie: Âge: 30
  print("Langages:", table.concat(luaTable.languages, ", ")) -- Sortie: Langages: Lua, JavaScript
end
```

### Encodage de Table Lua en JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Programmeur Lua",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Exemple de sortie pour l'encodage :
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Programmeur Lua"
}
```

Ces exemples simples démontrent comment travailler avec JSON en Lua, facilitant l'intégration d'applications Lua avec diverses technologies web et APIs externes. Rappelez-vous, bien que `dkjson` soit utilisé dans ces exemples, d'autres bibliothèques comme `cjson` et `RapidJSON` peuvent également être des alternatives appropriées selon les besoins de votre projet.
