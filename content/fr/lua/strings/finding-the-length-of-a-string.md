---
date: 2024-01-20 17:47:40.951067-07:00
description: "La longueur d'une cha\xEEne, c'est combien de caract\xE8res elle contient.\
  \ Pourquoi s'en soucier? Parce qu'on a souvent besoin de savoir o\xF9 on en est,\
  \ comme\u2026"
lastmod: '2024-03-13T22:44:57.915607-06:00'
model: gpt-4-1106-preview
summary: "La longueur d'une cha\xEEne, c'est combien de caract\xE8res elle contient.\
  \ Pourquoi s'en soucier? Parce qu'on a souvent besoin de savoir o\xF9 on en est,\
  \ comme\u2026"
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
La longueur d'une chaîne, c'est combien de caractères elle contient. Pourquoi s'en soucier? Parce qu'on a souvent besoin de savoir où on en est, comme pour valider une saisie ou couper une chaîne trop longue.

## How to:
Trouver la longueur d'une chaîne en Lua, c'est du gâteau. Utilisez l'opérateur `#` devant votre chaîne :

```lua
local chaine = "Salut tout le monde!"
local longueur = #chaine
print(longueur)  -- Affiche 20
```

Ou alors, avec la fonction `string.len` :

```lua
local chaine = "Bonjour"
local longueur = string.len(chaine)
print(longueur)  -- Affiche 7
```

## Deep Dive
Historiquement, l'opérateur `#` de Lua a toujours servi à avoir la taille d'une séquence, que ce soit pour les chaînes ou les tables. C'est rapide, direct.

En Lua, une alternative, c'est `string.len`. Pourquoi deux méthodes? `string.len` est plus explicite, c'est tout. L'opérateur `#` est plus court et plus utilisé.

Concernant les caractères Unicode, attention. Lua compte les octets, pas les caractères. Si votre chaîne a des caractères accentués ou autres exotiques, la longueur risque de ne pas être celle que vous attendez. Pour ça, il existe des bibliothèques externes ou des fonctions à créer.

## See Also
Pour approfondir vos connaissances en Lua :
- Documentation officielle sur les chaînes : [String Manipulation](http://www.lua.org/manual/5.4/manual.html#6.4)
