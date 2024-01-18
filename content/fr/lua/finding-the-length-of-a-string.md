---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Lua: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?
Trouver la longueur d'une chaîne (ou une série de caractères) est une tâche courante dans la programmation. Cela permet aux programmeurs de mesurer la taille d'une chaîne de caractères et de l'utiliser dans leur code.

## Comment faire:
Dans Lua, la fonction ```len()``` est utilisée pour trouver la longueur d'une chaîne. Elle prend en paramètre la chaîne de caractères et retourne la longueur de celle-ci. Voici un exemple de code avec un résultat de sortie:

```Lua
str = "Bonjour!"
print(len(str)) -- Résultat: 8
```

## Plongée en profondeur:
La fonction ```len()``` a été introduite dans Lua 5.1 et fait partie intégrante de la bibliothèque standard. Cependant, dans les versions antérieures de Lua, la longueur d'une chaîne pouvait être trouvée en utilisant la fonction ```#``` comme ceci: ```#str```. Il existe également d'autres alternatives pour trouver la longueur d'une chaîne, telles que l'utilisation de boucles ou de fonctions personnalisées. La fonction ```len()``` est optimisée pour une performance élevée et devrait être préférée pour trouver la longueur d'une chaîne dans Lua.

## Voir aussi:
- [Documentation officielle Lua sur la fonction len()](https://www.lua.org/manual/5.4/manual.html#pdf-string.len)
- [Autres méthodes pour trouver la longueur d'une chaîne en Lua](https://stackoverflow.com/questions/22655529/how-can-i-find-the-length-of-a-string-in-lua)