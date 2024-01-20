---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

**L'interpolation de chaînes** est une façon d'insérer des valeurs variables dans une chaîne fixe. Les programmeurs l'utilisent pour créer des messages dynamiques et facilement lisibles.

## Comment faire :

Voici un exemple simple de comment faire l'interpolation de chaînes en Lua avec la bibliothèque `string.format` :

```Lua
nom = "Jean"
message = string.format("Bonjour, %s!", nom)
print(message)  -- Output: Bonjour, Jean!
```

## Approfondissement 

Historiquement, Lua n'avait pas de fonction d'interpolation de chaînes comme celle qu'on trouve dans d'autres langages. À la place, on utilisait la méthode `string.format`. 

Cependant, il existe des alternatives à `string.format` telles que la méthode `..` qui peut aussi être utilisée pour concaténer des chaînes, comme ceci :

```Lua
nom = "Jean"
message = "Bonjour, "..nom.."!"
print(message)  -- Output: Bonjour, Jean!
```

Cependant, `string.format` est généralement préféré pour son expressivité et sa facilité d'utilisation.

## Voir aussi 

1. [Documentation Lua](https://www.lua.org/manual/5.3/) pour une connaissance plus profonde de `string.format` et des autres bibliothèques de chaînes de caractères en Lua.
2. [Guide de style Lua](https://github.com/Olivine-Labs/lua-style-guide) pour des bonnes pratiques en écriture de code Lua.