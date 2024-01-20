---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparer Deux Dates en Lua

## Quoi et Pourquoi?

En informatique, comparer deux dates c'est déterminer quel événement a eu lieu en premier. Les programmeurs font ça pour ordonner des événements, évaluer un laps de temps, ou gérer des délais.

## Comment faire:

En Lua, on utilise la fonction `os.time()` pour obtenir le temps en secondes depuis 1970, puis on compare ces valeurs.

```Lua
date1 = os.time({year=2021, month=7, day=20})
date2 = os.time({year=2023, month=1, day=15})

if date1 > date2 then
  print("La date1 est après la date2!")
elseif date1 < date2 then
  print("La date2 est après la date1!")
else
  print("Les deux dates sont identiques!")
end
```
Dans cet exemple, la sortie serait `"La date2 est après la date1!"` parce que vous comparez le 20 Juillet 2021 et le 15 Janvier 2023.

## Exploration approfondie

Lua peut comparer les dates parce qu'il les convertit en un nombre de secondes depuis un point de départ fixe, généralement le 1er janvier 1970. Cette méthode, qui vient du système d'exploitation Unix, rend la comparaison de dates aussi simple que la comparaison de nombres.

Il existe d'autres façons de comparer les dates en Lua, par exemple en utilisant le module `os.date`, ou en écrivant votre propre fonction.

Les détails de mise en œuvre de la comparaison de dates en Lua sont simples : La fonction `os.time` renvoie le nombre de secondes écoulées depuis 1970 pour une date donnée et ces valeurs peuvent être comparées avec les opérations traditionnelles.

## Voir aussi:

Pour plus d'informations, consultez ces sources :

1. Documentation Lua sur la bibliothèque 'os' : http://www.lua.org/manual/5.3/manual.html#6.9
2. Comparaison de dates et de temps dans Lua : https://stackoverflow.com/questions/16571087/lua-compare-dates-and-times
3. Le guide pratique de Lua : http://lua-users.org/wiki/PracticalLuaProgramming