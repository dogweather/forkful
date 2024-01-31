---
title:                "Travailler avec YAML"
date:                  2024-01-19
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, c'est quoi? Du texte lisible qui structure des données, comme le JSON mais plus simple. Pourquoi? Faciliter la configuration, l'échange de données et le debug. YAML, c'est cool, c'est clair.

## How to:
Pas de bibliothèque YAML intégrée en Lua, mais on peut utiliser `lyaml` pour faire le job. Installation avec `luarocks install lyaml`. Voici un petit exemple:

```Lua
local lyaml = require('lyaml')

-- Charger du YAML
local yaml_text = [[
- Poutine
- Baguette
- Fromage
]]

local data = lyaml.load(yaml_text)
print(data[1])  -- Affiche "Poutine"

-- Convertir en YAML
local lua_table = { "Pomme", "Orange", "Banane" }
local yaml_converted = lyaml.dump(lua_table)
print(yaml_converted)
```

Sortie:
```
Poutine
- Pomme
- Orange
- Banane
```

## Deep Dive
YAML est né en 2001, pour fusionner les points forts de plusieurs langages (XML, etc.). Alternatives? JSON pour la data, TOML pour les configs. Détaillé : YAML fait la map avec les tables Lua. Attention : complexité des types, respectez les bonnes pratiques.

## See Also
- YAML officiel : https://yaml.org
- lyaml sur GitHub : https://github.com/gvvaughan/lyaml
- LuaRocks lyaml : https://luarocks.org/modules/gvvaughan/lyaml
- JSON en Lua : https://www.json.org/json-fr.html
- TOML : https://toml.io/fr/
