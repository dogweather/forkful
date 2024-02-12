---
title:                "Analyser une date depuis une chaîne de caractères"
aliases:
- /fr/lua/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:49.186479-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Analyser une date à partir d'une chaîne de caractères consiste à convertir des représentations textuelles de dates et d'heures en un format qui peut être facilement manipulé, stocké ou comparé au sein d'un programme Lua. Les programmeurs effectuent cette tâche pour faciliter des opérations telles que la planification, la journalisation ou toute calcul temporel et pour combler le fossé entre les formats de dates lisibles par l'homme et les types de données structurées qu'un ordinateur peut traiter efficacement.

## Comment faire :
Lua n'a pas de support intégré pour la manipulation des dates et des heures au-delà de la fonctionnalité limitée fournie par les fonctions `os.date` et `os.time`. Cependant, celles-ci peuvent être utilisées pour un parsing de base, et pour des besoins plus complexes, la bibliothèque `luadate`, une bibliothèque externe, peut être utilisée.

**Utilisation de `os.date` et `os.time` :**
```lua
-- Convertir une date lisible par l'homme en un timestamp et inversement
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- Convertir le timestamp en un format lisible par l'homme
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- Sortie : 2023-09-21 15:00:00
```

**Utilisation de `luadate` (bibliothèque tierce) :**
Pour utiliser `luadate`, assurez-vous qu'elle est installée via LuaRocks ou votre gestionnaire de paquets de choix. `luadate` ajoute des capacités étendues d'analyse et de manipulation de dates et d'heures.

```lua
local date = require('date')

-- Analyser directement une chaîne de date
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- Sortie : 2023-09-21 15:00:00

-- Ajouter des durées
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- Sortie : 2023-09-28 15:00:00
```

La bibliothèque `luadate` offre une manière plus intuitive et puissante de travailler avec les dates, incluant l'analyse à partir de chaînes, le formatage, et les opérations arithmétiques sur les dates, ce qui simplifie considérablement le travail avec les données temporelles en Lua.
