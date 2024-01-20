---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Trouver la longueur d'une chaîne en programmation signifie déterminer le nombre de caractères qui la constituent. C'est une opération incontournable pour des tâches comme la validation d'entrée, la manipulation de textes, et bien d'autres.

## Comment faire :

Pour obtenir la longueur d'une chaîne en Lua, on utilise l'opérateur `#`. Simple, rapide et performant. Jetons un oeil sur un exemple.

```Lua
chaine = "Bonjour, tout le monde"
print(#chaine)
```

Dans la console, vous verrez :

```Lua
23
```

"Bonjour, tout le monde" comprend 23 caractères, y compris l'espace et la virgule.

## Plongeon en profondeur :

La fonction de calcul de longueur de chaîne Lua, bien qu'elle semble simple, cache une grande élégance sous sa modestie. Historiquement, Lua a toujours été prévu pour être un langage léger avec un accent sur l'efficacité, c'est pourquoi la fonction de longueur de chaîne est implémentée avec l'opérateur `#`, plutôt que comme une fonction standard.

L'opérateur `#` est généralisé en Lua : il fonctionne non seulement pour les chaînes, mais aussi pour les tables, un autre type de données fondamental en Lua. Notez que la longueur d'une table n'est pas toujours définie de manière unique en Lua.

En termes de détail d'implémentation, l'opérateur `#` donne la longueur d'une chaîne en temps constant, indépendamment de la longueur de la chaîne.

## Voir aussi :

Pour approfondir vos connaissances sur les chaînes de caractères en Lua, voici quelques liens utiles:

[Lua-Users Wiki: Strings Tutorial](http://lua-users.org/wiki/StringsTutorial)
[Lua Reference Manual: String Manipulation](http://www.lua.org/manual/5.1/manual.html#5.4)
[Pilgrim: Strings in Lua](https://www.pilgrim.me.uk/pilgrim-parts/strings-in-lua/)