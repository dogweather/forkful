---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
La génération de nombres aléatoires est un processus d'obtention de chiffres qui ne sont pas prévisibles, et pour lesquels tous les nombres ont une probabilité égale d'être obtenus. Les programmeurs le font pour des usages tels que les jeux, les simulations, et même pour la sécurité.

## Comment faire :
```Lua
math.randomseed(os.time())
local randomNumber = math.random(1, 100)
print(randomNumber)
```
Cela génèrera et affichera un nombre aléatoire entre 1 et 100 à chaque fois que vous exécutez le script.

## Plongeon en profondeur
La fonction `math.random` a été introduite dans Lua 3.0. Avant cela, les programmeurs avaient besoin d'implémenter leurs propres générateurs de nombres aléatoires.

It y a plusieurs alternatives à `math.random`. Par exemple, vous pouvez utiliser des bibliothèques externes pour obtenir des nombres aléatoires plus sophistiqués. L'une d'entre elles est pouvoir utiliser l'API `love.math.random` de LÖVE, si vous faites du développement avec ce framework.

La fonction `math.random` de Lua utilise l'algorithme de génération de nombres pseudo-aléatoires appelé Mersenne Twister, qui est réputé pour sa vitesse et sa qualité de nombres aléatoires.

## Voir aussi
[La documentation officielle de LUA pour math.random](https://www.lua.org/manual/5.1/manual.html#pdf-math.random)
[Le lien vers LÖVE, un framework de développement de jeux en Lua](https://love2d.org/)