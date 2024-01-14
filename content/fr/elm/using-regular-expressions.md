---
title:                "Elm: Utiliser les expressions régulières"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant dans le domaine de la programmation. Elles permettent de manipuler et de rechercher des motifs dans du texte, ce qui peut être très utile dans diverses situations, comme la validation de formulaires ou la manipulation de données. 

## Comment faire

Pour utiliser les expressions régulières en Elm, il faut d'abord les importer à partir du module `Regex` :

``` Elm
import Regex exposing (..)
```

Ensuite, on peut créer une expression régulière en utilisant la fonction `regex` et en lui passant une chaîne de caractères contenant le motif à rechercher :

``` Elm
let regEx = regex "elm"
```

On peut alors appliquer cette expression régulière à du texte en utilisant la fonction `find` :

``` Elm
let text = "J'aime programmer en Elm"
let result = find regEx text
```

La fonction `find` renverra un `Maybe Regex.Match` qui indique si le motif a été trouvé ou non, ainsi que les informations sur le premier match trouvé. On peut aussi utiliser `findAll` pour trouver tous les matches dans le texte.

## Plongée en profondeur

Les expressions régulières en Elm sont basées sur le standard standard ECMA-262, ce qui signifie qu'elles utilisent les mêmes symboles et caractères spéciaux que d'autres langages de programmation. On peut utiliser des symboles comme `.` pour représenter n'importe quel caractère, `+` pour indiquer un ou plusieurs occurrences, ou `?` pour indiquer que le caractère précédent est optionnel.

En plus de la recherche de motifs, les expressions régulières en Elm permettent également de remplacer des parties du texte en utilisant la fonction `replace` ou de diviser le texte en utilisant la fonction `split`.

## Voir aussi

- [La documentation officielle des expressions régulières en Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Un cours interactif sur les expressions régulières en Elm](https://elmprogramming.com/regex-in-elm.html)
- [Un outil en ligne pour tester vos expressions régulières en Elm](https://regex.guide/cookbook.html#elm)