---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?
La concatenation des chaînes est le processus de combiner deux ou plusieurs petites chaînes pour en faire une seule plus grande. Les programmeurs le font pour manipuler les données, générer des sorties dynamiques ou simplement pour économiser des ressources en évitant la création de nouvelles chaînes à chaque fois.

## Comment faire:
Pour concaténer des chaînes en Lua, utilisez l'opérateur `..`. Regardez l'exemple

```Lua
str1 = "Bonjour"
str2 = " le monde!"
print(str1..str2)
```
La sortie sera: `Bonjour le monde!`

Vous pouvez aussi concaténer des nombres avec des chaînes.

```Lua
nombre = 42
str = " a gagné le match."
print(nombre..str)
```
La sortie sera: `42 a gagné le match.`

## Plongée en profondeur
La concaténation de chaînes était une caractéristique courante des langages de programmation bien avant l’apparition de Lua en 1993. En Lua, l’unité mémoire fondamentale n’est pas le caractère, mais la chaîne. Cela permet une manipulation facile des chaînes, y compris leur concaténation.

Lua n'utilise pas `+` pour la concaténation pour éviter les erreurs de type lors du mélange des nombres et des chaînes. Au lieu de cela, Lua utilise `..` pour la concaténation.

Une alternative à l'opérateur `..` est la fonction `string.format()`. Elle est plus adaptée lorsque vous avez besoin de formats complexes.

En interne, Lua optimise la concaténation de chaînes en évitant les concaténations immédiates, ce qui permet d'économiser de la mémoire.

## Voir aussi :
Pour plus d'informations sur les chaînes dans Lua, vous pouvez consulter les liens ci-dessous:

1. Pilotage des chaînes Lua : https://www.lua.org/pil/2.4.html
2. Manipulation de chaînes Lua : https://www.tutorialspoint.com/lua/lua_strings.htm
3. Lua 5.4 référence manuelle - Chaînes: https://www.lua.org/manual/5.4/manual.html#6.1