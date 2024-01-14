---
title:                "Elm: Rechercher et remplacer du texte."
simple_title:         "Rechercher et remplacer du texte."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Souvent, lors de la programmation avec Elm, nous pouvons être confrontés à la tâche de rechercher et remplacer du texte dans notre code. Cela peut sembler fastidieux et chronophage, mais avec les bonnes connaissances et techniques, cela peut être fait efficacement et rapidement.

## Comment faire

Il existe plusieurs façons d'effectuer une recherche et un remplacement de texte dans Elm, mais la méthode la plus courante est d'utiliser la fonction `String.replace` de la bibliothèque `elm/string`. Voici un exemple de code pour remplacer toutes les occurrences d'un mot spécifique :

```Elm
import String exposing (replace)

texte = "Bonjour, comment ça va ?"
nouveauTexte = replace "Bonjour" "Salut" texte
-- résultat : "Salut, comment ça va ?"
```

Nous pouvons également utiliser des expressions régulières pour une recherche et un remplacement plus avancés. Voici un exemple qui va remplacer tous les espaces par des tirets dans une chaîne de caractères :

```Elm
import String exposing (replace, regex, Regex)

texte = "Elm est un langage de programmation fonctionnel"
regex = regex " " -- l'espace est notre motif de recherche
nouveauTexte = replace regex (Regex.andThen (\_ -> "-")) texte
-- résultat : "Elm-est-un-langage-de-programmation-fonctionnel"
```

Il est important de noter que les fonctions de recherche et de remplacement ne modifient pas la chaîne originale, mais retournent une nouvelle chaîne avec les modifications. Il est donc important d'assigner le résultat à une nouvelle variable, comme dans les exemples ci-dessus.

## Approfondissement

Maintenant que nous avons vu comment effectuer une recherche et un remplacement de base en Elm, penchons-nous sur quelques astuces et techniques supplémentaires.

Tout d'abord, si vous avez besoin de remplacer un grand nombre d'occurrences, il peut être plus efficace d'utiliser la fonction `toText` de la bibliothèque `elm/core` pour convertir votre chaîne en un type `Text` avant de faire les modifications, puis de la reconvertir en `String` à la fin. Cela peut améliorer considérablement les performances, surtout si votre chaîne est assez longue.

Deuxièmement, si vous êtes confronté à des caractères spéciaux ou unicode dans votre chaîne, vous devrez peut-être utiliser la fonction `String.fromChar` pour créer des `Char` à partir de ces caractères et les utiliser dans votre fonction de remplacement.

Enfin, si vous souhaitez effectuer une recherche et un remplacement sans tenir compte de la casse (majuscules ou minuscules), vous pouvez utiliser la fonction `String.toLower` pour convertir votre chaîne en minuscules avant et après la recherche et le remplacement.

## Voir aussi

Pour plus d'informations sur la fonction `String.replace` ainsi que d'autres fonctions utiles de la bibliothèque `elm/string`, vous pouvez consulter la documentation officielle ici : https://package.elm-lang.org/packages/elm/string/latest/.

Vous trouverez également des informations utiles sur l'utilisation des expressions régulières en Elm dans cet article de blog : https://elmprogramming.com/regex-in-elm.html.