---
title:                "Mettre une chaîne en majuscules"
html_title:           "Lua: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Capitaliser une chaîne signifie transformer toutes ses lettres en majuscules. Les programmeurs le font pour des raisons d'esthétique, de lisibilité et pour gérer uniformément les entrées des utilisateurs.

## Comment faire :

```Lua
--Voici un code simple pour capitaliser une chaîne en Lua
ma_chaine = "bonjour le monde"
ma_chaine = ma_chaine:upper()

print(ma_chaine) -- Affiche : BONJOUR LE MONDE
```
Dans cet exemple, nous utilisons la fonction `upper()` intégrée à Lua. Elle convertit toute la chaîne en majuscules.

## Approfondissement

1. **Contexte historique :**
   Avant l'introduction des méthodes standard `upper()`, les programmeurs Lua devaient écrire des boucles manuelles pour parcourir chaque caractère d'une chaîne et le convertir en majuscule, une tâche qui pouvait être fastidieuse.

2. **Alternatives :**
   En plus de `upper()`, Lua fournit également `lower()` pour convertir une chaîne en minuscules. Il existe également des fonctions pour manipuler la casse des lettres dans les chaînes.

3. **Détails d'implémentation :**
   La fonction `upper()` est une méthode intégrée de la bibliothèque de chaînes de Lua. Elle fonctionne en parcourant toute la chaîne et en remplaçant chaque caractère par son équivalent en majuscules.

## A Voir Aussi

Voici quelques ressources pour en savoir plus sur Lua et la manipulation des chaînes de caractères :

- [Manuel de référence Lua 5.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Guide LuaRocks sur les chaînes Lua](https://luarocks.org/modules/luarocks/lua-strings)
- [Gestion des chaînes en Lua - Tutorialspoint](https://www.tutorialspoint.com/lua/lua_strings.htm)