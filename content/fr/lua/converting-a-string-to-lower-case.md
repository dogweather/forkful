---
title:                "Conversion d'une chaîne de caractères en minuscules"
html_title:           "Lua: Conversion d'une chaîne de caractères en minuscules"
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Convertir une chaîne de caractères en minuscules est un processus de conversion de tous les caractères d'une chaîne en utilisant les mêmes caractères mais en minuscules. Les programmeurs font souvent cela pour faciliter la comparaison de chaînes de caractères et pour s'assurer que les données entrées par les utilisateurs sont uniformes.

## Comment faire:

```
local str = "Bonjour, LUA!"
local lower_str = str:lower()

print(lower_str) -- Output: bonjour, lua!
```

Dans cet exemple, nous définissons d'abord une variable contenant notre chaîne de caractères, puis nous utilisons la méthode ```:lower()``` pour la convertir en minuscules. Finalement, nous imprimons le résultat en utilisant la fonction ```print()```.

## Plongée en profondeur:

La conversion de chaînes en minuscules est une fonctionnalité courante dans de nombreux langages de programmation, y compris LUA. Il existe également d'autres façons de réaliser cette tâche, telles que l'utilisation de fonctions intégrées comme ```string.lower()``` ou en utilisant des expressions régulières.

L'implémentation de la conversion en minuscules dans LUA utilise une table de correspondance pour remplacer chaque caractère majuscule par son équivalent en minuscule. Il est également possible de personnaliser cette table de correspondance pour prendre en compte les caractères spéciaux ou les langues étrangères.

## Voir aussi:

Pour en savoir plus sur les différentes fonctions liées aux chaînes de caractères en LUA, consultez la documentation officielle sur les fonctions de chaînes: https://www.lua.org/manual/5.4/manual.html#6.4.

Si vous recherchez des alternatives à la conversion en minuscules, vous pouvez également explorer les autres méthodes de comparaison de chaînes telles que l'utilisation de la méthode ```:find()``` ou l'utilisation de la bibliothèque externe ```lstring``` pour des fonctionnalités plus avancées.