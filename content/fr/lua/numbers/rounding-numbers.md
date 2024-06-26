---
date: 2024-01-26 03:45:32.320720-07:00
description: "Comment faire : Lua n'inclut pas de fonction d'arrondi directement,\
  \ contrairement \xE0 certains autres langages. Historiquement, vous devez \xE9crire\
  \ la v\xF4tre\u2026"
lastmod: '2024-04-05T21:53:59.404079-06:00'
model: gpt-4-0125-preview
summary: "Lua n'inclut pas de fonction d'arrondi directement, contrairement \xE0 certains\
  \ autres langages."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
```lua
-- L'arrondi de base en Lua n'est pas intégré de base, mais vous pouvez définir une fonction :

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Pour arrondir à un nombre spécifique de décimales :
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Exploration approfondie
Lua n'inclut pas de fonction d'arrondi directement, contrairement à certains autres langages. Historiquement, vous devez écrire la vôtre ou utiliser une bibliothèque tierce. Les solutions courantes reposent sur `math.floor()` pour arrondir vers le bas et `math.ceil()` pour arrondir vers le haut, couplé à l'ajout ou à la soustraction de 0.5 avant de le faire, en fonction du signe du nombre.

Les alternatives à la création de votre propre fonction comprennent des bibliothèques telles que "lua-users wiki" ou "Penlight". Chacune a ses avantages et inconvénients, comme des fonctionnalités supplémentaires ou plus de surcharge.

En interne, ces fonctions fonctionnent normalement en exploitant la manière dont les ordinateurs stockent les nombres à virgule flottante. Ajouter 0.5 à un flottant positif que vous souhaitez arrondir le poussera au-delà du seuil de la valeur entière suivante, donc lorsque vous appliquez `math.floor()`, il arrondit vers le bas à cet entier le plus proche.

## Voir aussi
- [Manuel de référence Lua 5.4 : Les fonctions mathématiques](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Bibliothèques Lua Penlight : Math](https://github.com/lunarmodules/Penlight)
