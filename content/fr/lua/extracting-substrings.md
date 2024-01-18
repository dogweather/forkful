---
title:                "Extraction de sous-chaînes"
html_title:           "Lua: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Extraire des sous-chaînes est une technique de programmation qui consiste à récupérer une partie d'une chaîne de caractères à partir d'une position de départ et d'une longueur spécifiées. Les programmeurs utilisent souvent cette méthode pour traiter de grandes quantités de données de manière plus efficace.

## Comment faire:
Voici un exemple simple en Lua montrant comment extraire une sous-chaîne à partir d'une chaîne donnée:
```Lua
str = "Bonjour tout le monde"
print(string.sub(str, 4, 10))
```
Cela devrait afficher "jour t" en sortie car nous avons spécifié que la sous-chaîne commence à la 4ème position et se termine à la 10ème position.

## Plongée en profondeur:
Extraction de sous-chaînes a été introduit pour la première fois dans le langage informatique APL en 1962. Depuis lors, de nombreux autres langages, y compris Lua, ont adopté cette méthode en tant que fonction intégrée.

Alternativement, les programmeurs peuvent également utiliser des boucles et des conditions pour extraire manuellement des sous-chaînes à partir d'une chaîne donnée, mais cela peut être fastidieux et moins efficace en termes de performances.

La fonction ```string.sub()``` utilisée pour extraire des sous-chaînes prend en compte plusieurs paramètres tels que la position de départ, la position de fin, la répétition et la valeur par défaut pour les sous-chaînes manquantes.

## Voir aussi:
Pour en savoir plus sur les fonctions disponibles pour traiter les chaînes de caractères en Lua, vous pouvez consulter la documentation officielle: https://www.lua.org/pil/20.2.html