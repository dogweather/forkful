---
date: 2024-01-20 17:33:15.049543-07:00
description: "Comment faire : Historiquement, la gestion des dates en informatique\
  \ a toujours \xE9t\xE9 fondamentale, n\xE9cessitant des standards de formats et\
  \ de calculs\u2026"
lastmod: '2024-04-05T22:51:11.911699-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la gestion des dates en informatique a toujours \xE9t\xE9\
  \ fondamentale, n\xE9cessitant des standards de formats et de calculs pr\xE9cis."
title: Comparer deux dates
weight: 27
---

## Comment faire :
```Lua
os.date("*t") -- Récupère la date et l'heure actuelles.

local date1 = os.time({year=2023, month=4, day=15})  -- Définit une date spécifique.
local date2 = os.time({year=2023, month=4, day=18})  -- Définit une autre date.

if date1 < date2 then
    print("La date1 est antérieure à la date2.")
elseif date1 > date2 then
    print("La date1 est postérieure à la date2.")
else
    print("Les deux dates sont identiques.")
end
```
Sortie échantillon:
```
La date1 est antérieure à la date2.
```

## Deep Dive
Historiquement, la gestion des dates en informatique a toujours été fondamentale, nécessitant des standards de formats et de calculs précis. En Lua, `os.time` et `os.date` sont des fonctions de base pour manipuler des dates. On peut comparer deux timestamps (le nombre de secondes depuis une date de référence) directement. Alternativement, on pourrait aussi utiliser des bibliothèques dédiées comme `luadate` qui offrent plus de fonctionnalités. Une contrainte à considérer : la gestion des fuseaux horaires, qui requiert parfois des calculs complémentaires.

## Voir aussi
- Documentation Lua sur les fonctions de date/temps: https://www.lua.org/manual/5.4/manual.html#6.9
- GitHub du module LuaDate, pour des fonctionnalités étendues : https://github.com/Tieske/date
