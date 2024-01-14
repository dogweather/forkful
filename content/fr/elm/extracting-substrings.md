---
title:                "Elm: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une tâche courante en programmation Elm, et c'est un outil puissant à ajouter à votre boîte à outils de développeur. Que vous travailliez sur un projet de traitement de texte ou sur un programme de traitement de données, savoir comment extraire des sous-chaînes dans Elm peut être très utile.

## Comment faire

Pour extraire des sous-chaînes en Elm, nous allons utiliser la fonction `substring` qui se trouve dans le module `String`. Cette fonction prend en argument une chaîne de caractères et deux indices, représentant respectivement le début et la fin de la sous-chaîne que vous souhaitez extraire. Voici un exemple de code Elm qui illustre l'utilisation de `substring` :

```Elm
import String

exemple = "Bonjour Elm"

sousChaine = String.substring 7 11 exemple

-- "Elm" sera la valeur retournée par sousChaine
```

Comme vous pouvez le voir dans cet exemple, nous avons d'abord importé le module `String`, puis nous avons créé une variable contenant notre chaîne de caractères. Ensuite, en utilisant `substring`, nous avons extrait la sous-chaîne "Elm" en utilisant les indices 7 et 11 pour spécifier les caractères que nous voulions. Vous pouvez également utiliser des variables pour ces indices, tant qu'elles sont de type `Int`.

## Plongée en profondeur

Il est important de noter que les indices donnés à la fonction `substring` doivent être valides. Cela signifie qu'ils doivent être compris entre 0 et la longueur de la chaîne de caractères moins 1. Si les indices donnés ne sont pas valides, la fonction retournera une erreur. De plus, `substring` ne modifie pas la chaîne de caractères d'origine, elle en renvoie simplement une nouvelle.

Il est également possible d'utiliser un indice négatif pour le début de la sous-chaîne. Dans ce cas, l'index sera compté à partir de la fin de la chaîne de caractères. Par exemple, `-2` serait l'avant-dernier caractère de la chaîne. Cela peut être très utile si vous avez besoin d'extraire une sous-chaîne à partir d'une position inconnue dans la chaîne de caractères.

## Voir aussi

Pour en savoir plus sur les fonctions de manipulation de chaînes en Elm, consultez la documentation officielle du module `String` : https://package.elm-lang.org/packages/elm/core/latest/String