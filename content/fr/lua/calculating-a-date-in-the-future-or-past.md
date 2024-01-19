---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Lua: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé signifie déterminer une date précise en ajoutant ou en soustrayant un certain nombre de jours, semaines, mois, etc. à une date donnée. Les programmeurs le font pour gérer des tâches liées au temps, comme le suivi de l'expédition, rappels d'événements, etc.

## Comment Faire:
Avec Lua, on utilise le module `os`. Voici un exemple qui ajoute 7 jours à la date courante :

```Lua
os.execute("sleep " .. 3) -- attend 3 secondes
date_courante = os.time()
dans_une_semaine = os.date('%c', date_courante + (7 * 24 * 60 * 60))
print(dans_une_semaine)
```

La sortie ressemblera à :

```Lua
Tue Sep 14 13:01:28 2021
```

## Deep Dive
Historiquement, calculer une date dans le futur ou le passé était un processus ardu à cause des variations d'heure et de jour. Mais avec les modules `os` et `os.time` en Lua, la tâche est plus simple.

Alternativement, on pourrait utiliser le module `luatz` qui offre une gestion plus complète du temps et des fuseaux horaires. Cependant, pour des tâches simples, l'utilisation d'`os` est suffisante.

Les dates en Lua sont représentées en secondes depuis le 1er janvier 1970. Donc, quand vous voulez ajouter ou soustraire des jours, vous devez les convertir en secondes (24 heures * 60 minutes * 60 secondes).

## Voir Aussi
1. Documentation Lua 'os' module: [https://www.lua.org/manual/5.3/manual.html#6.9](https://www.lua.org/manual/5.3/manual.html#6.9)
2. Library LuaTZ: [https://github.com/daurnimator/luatz](https://github.com/daurnimator/luatz)
3. Tutorial on working with dates and times in Lua: [http://lua-users.org/wiki/DateAndTime](http://lua-users.org/wiki/DateAndTime)