---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:39.509715-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Générer des nombres aléatoires, c'est comme lancer un dé virtuel : on obtient une valeur imprévisible. Les programmeurs s'en servent pour tout, des jeux aux simulations, en passant par les tests de logiciel.

## Comment faire :
```Lua
-- D'abord, on initialise le générateur
math.randomseed(os.time())

-- Pour un nombre aléatoire entre 1 et 100
local nombre = math.random(1, 100)
print(nombre)

-- Pour un nombre flottant aléatoire entre 0 et 1
local flottant = math.random()
print(flottant)
```
Sample output:
```
42
0.7345975135794
```

## Plongeon en profondeur
Avant Lua 5.1, le générateur de nombres aléatoires n'était pas fiable. Avec la version actuelle, `math.randomseed` utilise `os.time()` pour de meilleurs résultats. Il existe d’autres méthodes, comme le Mersenne Twister, mais pour la simplicité et beaucoup de besoins, math.random fait l'affaire. L'important est de se rappeler d'initialiser le générateur avec `math.randomseed` sinon, vous obtiendrez la même séquence de nombres à chaque fois.

## Voir aussi
- La documentation officielle de Lua : https://www.lua.org/manual/5.4/
- Une discussion sur les générateurs de nombres aléatoires : https://stackoverflow.com/questions/20152105/what-random-seed-to-use-in-lua
- Informations sur le Mersenne Twister (en anglais) : https://en.wikipedia.org/wiki/Mersenne_Twister