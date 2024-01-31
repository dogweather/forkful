---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:40.247961-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Obtenir la date actuelle en programmation, c'est récupérer la date et l'heure à l'instant où le code s'exécute. Les développeurs font cela pour enregistrer des événements, gérer des tâches planifiées ou tout simplement afficher la date et l'heure à l'utilisateur.

## Comment faire :

En Lua, la récupération de la date et de l'heure actuelles est simple avec la fonction `os.date`. Voici un exemple :

```lua
local date_actuelle = os.date("%Y-%m-%d")
print("La date d'aujourd'hui est : " .. date_actuelle)

local heure_actuelle = os.date("%H:%M:%S")
print("L'heure actuelle est : " .. heure_actuelle)
```

Sortie possible:
```
La date d'aujourd'hui est : 2023-04-01
L'heure actuelle est : 14:23:45
```

## Exploration Approfondie :

Historiquement, Lua a souvent été utilisé pour des jeux vidéo et des scripts intégrés, où la gestion du temps peut être cruciale. La fonction `os.date` est basée sur la fonction C standard `strftime`, donc elle est très performante.

Alternatives: Pour des besoins plus spécifiques, comme des manipulations plus complexes de dates, on peut se tourner vers des bibliothèques externes comme `luadate`.

Détails d’implémentation : `os.date("*t")` donne une table avec tous les composants de la date et de l'heure, ce qui permet de les manipuler avec une plus grande finesse.

## Voir Aussi :

- La documentation Lua sur la bibliothèque `os`: https://www.lua.org/manual/5.4/manual.html#6.9
- ‘luadate’, une bibliothèque de dates pour Lua : https://github.com/Tieske/date
