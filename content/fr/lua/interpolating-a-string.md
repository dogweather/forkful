---
title:                "Interpoler une chaîne de caractères"
html_title:           "Lua: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent-ils?

L'interpolation de chaîne en Lua fait référence à la capacité de concaténer ou de combiner des chaînes de caractères avec des valeurs dynamiques. Les programmeurs utilisent cela pour faciliter la création de chaînes de caractères complexes avec des valeurs variables.

## Comment faire:

```Lua
-- Exemple 1:
local name = "John"
print("Bonjour, " .. name .. "!") -- Output: Bonjour, John!

-- Exemple 2:
local age = 25
print("J'ai " .. age .. " ans.") -- Output: J'ai 25 ans.
```

## Plongée en profondeur:

L'interpolation de chaîne a été introduite dans Lua 5.0, permettant aux programmeurs de combiner facilement des valeurs dynamiques dans des chaînes de caractères. Il existe d'autres méthodes pour concaténer des chaînes de caractères, telles que l'utilisation de la fonction ```table.concat()```, mais l'interpolation de chaîne offre une solution plus simple et plus intuitive. Cela peut également être utile lors de la création de longues chaînes de caractères avec plusieurs valeurs variables.

## Voir aussi:

- Tutoriel Lua : https://learnxinyminutes.com/docs/fr-fr/lua-fr/
- Documentation officielle de Lua : https://www.lua.org/docs.html