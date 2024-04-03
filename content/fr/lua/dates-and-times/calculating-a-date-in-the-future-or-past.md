---
date: 2024-01-20 17:31:19.972807-07:00
description: 'Comment faire : En Lua, on utilise souvent `os.date` et `os.time`. Voici
  un exemple basique .'
lastmod: '2024-03-13T22:44:57.951226-06:00'
model: gpt-4-1106-preview
summary: En Lua, on utilise souvent `os.date` et `os.time`.
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
En Lua, on utilise souvent `os.date` et `os.time`. Voici un exemple basique :

```Lua
local aujourd_hui = os.time() -- Temps actuel en secondes
local jours = 5

-- Calculer une date dans le futur (+) ou dans le passé (-)
local futur = os.date("*t", aujourd_hui + (jours * 86400))
print("Dans " .. jours .. " jours, on sera le " .. os.date("%d/%m/%Y", os.time(futur)))
```

Sortie possible :
```
Dans 5 jours, on sera le 12/04/2023
```

Pour retrancher des jours :

```Lua
local passe = os.date("*t", aujourd_hui - (jours * 86400))
print("Il y a " .. jours .. " jours, on était le " .. os.date("%d/%m/%Y", os.time(passe)))
```

Sortie possible :
```
Il y a 5 jours, on était le 02/04/2023
```

## Pour aller plus loin :
Historiquement, gérer les dates en programmation a toujours été un peu casse-tête à cause des fuseaux horaires et des années bissextiles. Lua ne fait pas exception, mais offre une certaine simplicité avec `os.date` et `os.time`.

Des alternatives incluent l'utilisation de bibliothèques tierces comme `luadate`, qui a des fonctions plus avancées pour manipuler les dates.

En termes de mise en œuvre, on effectue souvent des calculs en se basant sur des secondes puisqu'une date en Lua est simplement le nombre de secondes depuis l'époque (1er janvier 1970). C'est ce qu'on appelle un timestamp.

## Voir Aussi :
- La documentation de Lua sur les fonctions `os.date` et `os.time`: https://www.lua.org/manual/5.4/manual.html#6.9
- LuaDate, une bibliothèque de manipulation de dates : https://github.com/Tieske/date
- Gestion des fuseaux horaires : https://stackoverflow.com/a/16946812/2924421
