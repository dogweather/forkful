---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

La conversion d'une date à partir d'une chaîne consiste à transformer une chaîne de texte représentant une date sous une forme particulière en un objet de date pouvant être manipulé en Lua. Les programmeurs le font pour exploiter des informations plus complexes à partir de chaînes de date et pour réaliser des opérations comme le calcul de la différence entre deux dates.

## Comment faire:

La bibliothèque os.date de Lua peut être utilisée pour analyser une date à partir d'une chaîne. Voici un exemple sur la façon de faire:

```Lua
local pattern = "(%d+)-(%d+)-(%d+)"
local y, m, d = string.match("2023-03-01", pattern)
print(os.time({year=y, month=m, day=d}))
```

Dans cet exemple, la méthode os.time est utilisée pour convertir les chaînes "y", "m" et "d" extraites de la chaîne de date en un temps de date Unix. L'output sera:

```Lua
1690924800
```

## Plongeon en Profondeur

Historiquement, Lua n'incluait pas de fonctions natives pour l'analyse de dates et de temps, ce qui laissait les développeurs dépendants des bibliothèques tierces pour effectuer ces tâches. Cependant, la fonction os.time a été introduite dans Lua 5.2 pour aider à pallier ce manque.

Une autre alternative pour analyser une date à partir d'une chaîne en Lua est l'utilisation de la bibliothèque luadate. Celle-ci fournit des fonctions plus avancées pour manipuler les dates et les temps. Toutefois, il est important de noter que cette bibliothèque n'est pas une partie standard de Lua et doit donc être installée séparément.

Tracez vos chaînes de date avec soin! Un mauvais format de date et d'heure peut causer une erreur d'exécution. Par conséquent, assurez-vous toujours que la chaîne de date que vous passez à os.time correspond au format défini dans le patron de chaîne. 

## Voir Aussi

Lua 5.2 Documentation: http://www.lua.org/manual/5.2/

LuaDate Documentation: https://github.com/Tieske/date

Tutorial on Lua's Time and Date: https://www.tutorialspoint.com/lua/lua_date_time.htm