---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitaliser une chaîne signifie transformer toutes les lettres en majuscules. Les programmeurs utilisent la capitalisation pour normaliser les entrées texte ou mettre en évidence des éléments importants.

## How to:
```Lua
-- Capitalisation d'une chaîne de caractères en Lua
local texte = "bonjour, le monde!"
local texte_capitalise = texte:upper()

print(texte_capitalise)  -- Affiche: "BONJOUR, LE MONDE!"
```
Résultat : `BONJOUR, LE MONDE!`

## Deep Dive
Historiquement, capitaliser une chaîne en Lua est facilité par la méthode `upper`. Cette fonctionnalité existe depuis les premières versions, montrant que la manipulation des chaînes de caractères est un besoin de base en programmation.

Des alternatives? Oui. Utiliser des boucles et travailler caractère par caractère, mais c'est plus complexe. Côté performance, `upper` est optimisée; donc, c'est le choix judicieux.

Détails d'implémentation? La standardisation des encodages texte, comme UTF-8, influe sur la gestion des majuscules/minuscules dans un contexte international. Lua traite correctement les lettres accentuées dans la majorité des cas, mais pour des alphabets non-latins, des libraries supplémentaires peuvent être requises.

## See Also
- Documentation Lua sur les chaînes de caractères : https://www.lua.org/manual/5.4/manual.html#6.4
- Forum Lua : https://www.lua.org/forums.html
- Pour les questions d'encodage en Lua, le module `luautf8` peut aider : https://github.com/starwing/luautf8
