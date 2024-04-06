---
date: 2024-01-20 17:34:59.859798-07:00
description: "Comment faire : Historiquement, la concat\xE9nation \xE9tait l'une des\
  \ rares mani\xE8res d'assembler des textes dans la plupart des langages de programmation,\
  \ Lua\u2026"
lastmod: '2024-04-05T21:53:59.400679-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la concat\xE9nation \xE9tait l'une des rares mani\xE8res\
  \ d'assembler des textes dans la plupart des langages de programmation, Lua inclus."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## Comment faire :
```lua
-- Concaténation simple avec l'opérateur '..'
local bonjour = "Salut"
local monde = "le monde"
local phrase = bonjour .. ", " .. monde .. "!"
print(phrase)  -- Salut, le monde!

-- Concaténation avec la fonction table.concat
local mots = {"Lua", "est", "super"}
local phrase_complete = table.concat(mots, " ")
print(phrase_complete)  -- Lua est super

-- Utilisation de la méthode string.format pour une concaténation formatée
local nom = "Mireille"
local age = 30
local presentation = string.format("%s a %d ans.", nom, age)
print(presentation)  -- Mireille a 30 ans.
```

## Deep Dive
Historiquement, la concaténation était l'une des rares manières d'assembler des textes dans la plupart des langages de programmation, Lua inclus. Les alternatives incluent le tamponnage de chaînes avec des tableaux ou le formatage de chaînes, qui peut être plus performant ou offrir plus de contrôle sur le format du texte respectivement. Lua utilise de l'optimisation interne pour la concaténation avec '..' pour éviter des problèmes de performance.

## Voir Aussi
- Lua 5.4 Reference Manual : String Manipulation - https://www.lua.org/manual/5.4/manual.html#6.4
- Programming in Lua (4th edition) – Aspects avancés sur la gestion des chaînes - https://www.amazon.com/Programming-Lua-Fourth-Roberto-Ierusalimschy/dp/8590379868/
- Lua Users Wiki - String Recipes - http://lua-users.org/wiki/StringRecipes
