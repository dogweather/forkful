---
title:                "Utilisation des expressions régulières"
html_title:           "Lua: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Une expression régulière est une séquence de caractères qui est utilisée pour trouver des motifs dans une chaîne de texte. Les programmeurs utilisent des expressions régulières pour rechercher et manipuler des données textuelles de manière efficace.

## Comment faire :

```Lua
-- Créer une expression régulière pour trouver toutes les occurrences de "chat" dans une chaîne de texte

local txt = "J'aime les chats, les chiens et les oiseaux."
local pattern = "chat"
local matches = string.gmatch(txt, pattern)

for match in matches do
  print(match)
end
```

Résultat :
```
chat
```

## Plongée en profondeur :

(1) Les expressions régulières ont été inventées dans les années 1950 par le mathématicien Stephen Kleene pour décrire des langages formels.

(2) Bien qu'elles soient largement utilisées, il existe d'autres méthodes pour traiter les données textuelles, telles que les algorithmes de recherche ou les analyseurs lexicaux.

(3) Lua utilise le module string pour gérer les expressions régulières, qui sont basées sur les expressions régulières POSIX.

## Voir aussi :

- [Autre tutoriel sur les expressions régulières en Lua](https://www.lua.org/pil/20.2.html)