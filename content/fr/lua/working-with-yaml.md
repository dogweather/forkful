---
title:                "Travailler avec yaml"
html_title:           "Lua: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

# Qu'est-ce que YAML et pourquoi les programmeurs l'utilisent-ils?

YAML est un format de données basé sur le texte conçu pour être facile à lire et à écrire pour les humains, ainsi que facile à analyser et à générer pour les machines. Les programmeurs l'utilisent souvent pour stocker et échanger des données structurées telles que des configurations de programmes ou des données de jeu.

# Comment faire:

Voici un exemple de code en Lua pour écrire et lire des données YAML:

```Lua
local yaml = require("yaml")

-- Définir des données
local data = {
  fruits = {"pomme", "banane", "orange"},
  nombre = 5,
  est_vrai = true
}

-- Convertir en format YAML
local yaml_data = yaml.dump(data)
print(yaml_data)

--[[
 fruits:
  - pomme
  - banane
  - orange
 nombre: 5
 est_vrai: true
]]

-- Charger des données YAML et les convertir en table Lua
local loaded_data = yaml.load(yaml_data)
for key, value in pairs(loaded_data) do
  print(key, value)
end

--[[
 fruits	table: 0x7f98480062e0
 nombre	5
 est_vrai	true
]]
```

# Plongée en profondeur:

YAML a été créé en 2001 par Clark Evans et Ingy döt Net, et son nom signifie "YAML Ain't Markup Language". Il offre une alternative plus lisible et plus légère aux formats de données tels que JSON. En plus de Lua, de nombreux autres langages de programmation prennent en charge le traitement de YAML, comme Python ou JavaScript.

Pour utiliser YAML en Lua, vous pouvez installer une bibliothèque comme "lua-yaml" ou "lua-resty-yaml" en utilisant un gestionnaire de paquets tel que LuaRocks. Vous pouvez également utiliser la bibliothèque standard "yaml" dans Lua 5.3 et versions ultérieures.

# Voir aussi:

- [Site officiel de YAML](https://yaml.org/)