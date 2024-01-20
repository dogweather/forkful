---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

JSON (JavaScript Object Notation) est un format de données simple pour échanger des données entre serveurs et applications web. Les programmeurs l'utilisent pour sa facilité de lecture et d'écriture par des humains, ainsi que pour sa simplicité d'analyse et de génération par des machines.

## Comment faire :

Pour travailler avec JSON en Lua, tu as besoin d'un module externe. `dkjson` est souvent utilisé. Voici comment :

1. Installation de `dkjson`:

Tu peux télécharger `dkjson` ou utiliser luarocks :

```Lua
luarocks install dkjson
```

2. Utilisation de `dkjson`:

```Lua
local dkjson = require 'dkjson'

-- Conversion d'une table Lua en chaîne JSON
local lua_table = { nom = "Dupont", age = 42, email = "dupont@example.com" }
local json_string = dkjson.encode(lua_table)
print(json_string)  -- {"age":42,"email":"dupont@example.com","nom":"Dupont"}

-- Analyse d'une chaîne JSON en table Lua
local json_text = '{"nom":"Dupont","age":42,"email":"dupont@example.com"}'
local table, pos, err = dkjson.decode(json_text, 1, nil)
if err then
  print ("Erreur:", err)
else
  print(table.nom)  -- Dupont
end
```

3. Sortie d'échantillon :

```Lua
{"age":42,"email":"dupont@example.com","nom":"Dupont"}
Dupont
```

## Plongée profonde :

JSON est né de la nécessité de communiquer des objets JavaScript entre le client et le serveur. Des alternatives comme XML étaient plus lourdes. Bien que dérivé de JavaScript, JSON est indépendant du langage. En Lua, `dkjson` et `cjson` sont des modules populaires. `cjson` est plus rapide que `dkjson` mais moins flexible. En choisissant un module, considère la performance et la compatibilité.

## Voir aussi :

Voici quelques ressources pour approfondir :

- Documentation `dkjson`: http://dkolf.de/src/dkjson-lua.fsl/home
- `cjson` sur GitHub: https://github.com/mpx/lua-cjson
- Spécifications JSON: http://www.json.org/json-fr.html
- Tutoriel Lua: https://www.lua.org/pil/contents.html