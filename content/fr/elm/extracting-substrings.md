---
title:                "Elm: Extraire des sous-chaînes"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes peut sembler être une tâche mineure en programmation Elm, mais elle peut en réalité être très utile dans de nombreuses situations. Cela peut vous aider à manipuler des chaînes de caractères plus facilement, à remplacer des éléments spécifiques ou à effectuer des opérations de validation complexe.

## Comment faire

Il existe plusieurs façons d'extraire des sous-chaînes en Elm, mais la méthode la plus courante est d'utiliser la fonction `String.slice`. Voici un exemple :

```Elm
myString = "Technologie Elm"
substring = String.slice 11 14 myString
```

Cet exemple extrait les caractères de la position 11 à 14 dans la chaîne `myString`, ce qui donne comme résultat la sous-chaîne "Elm".

Il est également possible d'utiliser les fonctions `String.take` et `String.drop` pour extraire une partie spécifique d'une chaîne. Par exemple :

```Elm
myString = "Coding est amusant"
firstFiveCharacters = String.take 5 myString
```

Cela créera une nouvelle chaîne contenant les cinq premiers caractères de `myString` : "Coding".

## Plongée profonde

L'une des fonctionnalités les plus intéressantes de l'extraction de sous-chaînes en Elm est qu'elle peut également être utilisée avec des tableaux. Par exemple, si vous avez un tableau de chaînes de caractères et que vous souhaitez extraire une sous-chaîne spécifique de chaque élément, vous pouvez utiliser la fonction `List.map` combinée à `String.slice`. Voici un exemple :

```Elm
array = ["Je suis heureux", "Faire du sport", "Programmer est amusant"]
updatedArray = List.map (\string -> String.slice 0 7 string ) array
```

Cet exemple utilisera `String.slice` pour extraire les sept premiers caractères de chaque élément du tableau `array`. Le résultat sera un nouveau tableau contenant les éléments suivants : ["Je suis", "Faire du", "Program"].

## Voir aussi

Pour en savoir plus sur les fonctions de manipulation de chaînes en Elm, vous pouvez consulter la documentation officielle : https://guide.elm-lang.org/appendix/strings.html

Vous pouvez également découvrir d'autres astuces de programmation Elm sur les blogs et sites suivants :

- https://medium.com/codeurs-de-louest/comment-devenir-un-expert-en-programmation-en-elm-c8f5501c3dba
- https://freecontent.manning.com/composing-functions-in-elm/
- https://medium.com/the-z/mise-en-place-et-structures-de-projet-elm-b4196c80b564