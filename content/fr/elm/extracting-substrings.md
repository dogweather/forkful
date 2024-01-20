---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Extraire des sous-chaînes revient à prendre une partie d'une chaîne existante. Les programmeurs font cela pour manipuler et analyser les données textuelles.

## Comment faire :

```Elm
import String

exampleString = "Bonjour, le monde!"

substring = String.slice 0 7 exampleString

```
Ce qui donne "Bonjour".

```Elm
anotherExample = String[left](7 13 exampleString)
```
Ce qui donne "le monde".

## Plongée en profondeur

L'extraction de sous-chaînes existe depuis que la programmation a été inventée. Dans Elm, nous utilisons la fonction `String.slice` pour cela. 

Les alternatives comprennent l'utilisation de bibliothèques de manipulation de chaînes tierces qui peuvent offrir plus de flexibilité, mais avec un coût d'apprentissage supplémentaire.

Le `String.slice` fonctionne en parcourant la chaîne une fois et en copiant les caractères entre les indices de début et de fin.

## Voir aussi

Pour plus d'informations sur les fonctions de chaîne en Elm, consultez la documentation officielle ici: [Elm string library](https://package.elm-lang.org/packages/elm/core/latest/String). Aussi, le livre ["Programmer en Elm"](https://pragprog.com/titles/jzenelm/programming-elm/) offre une excellente introduction à la programmation en Elm.