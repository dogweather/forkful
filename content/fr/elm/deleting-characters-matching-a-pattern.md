---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

Supprimer les caractères correspondant à un certain modèle est un processus utilisé pour nettoyer ou formatiser des chaînes de texte. Les programmeurs font cela pour manipuler des données et faciliter le traitement ultérieur.

## Comment faire :

Dans Elm, nous utilisons la fonction `String.replace`. Voyons un exemple.

```Elm
import Html exposing (text)
import String

main =
    text <|
        String.replace "a" "" "ananas" -- remplace 'a' par '' dans 'ananas'
```

Dans cet exemple, le résultat sera `nns`, car tous les caractères `a` ont été supprimés de la chaîne `ananas`.

## Plongeon Profond 

Historiquement, cette fonction a été introduite lors de la sortie d'Elm 0.18 parce que les gens avaient besoin d'une manière de nettoyer et manipuler des cordes de texte. Alternativement, vous pouvez utiliser des expressions régulières, mais Elm n'a pas de support intégré pour cela. Si vous devez supprimer plusieurs caractères, vous pouvez utiliser `String.foldl` pour effectuer plusieurs remplacements sur une chaîne.

Dans les détails de mise en œuvre, `String.replace` est réellement défini à l'aide d'une fonction JavaScript sous-jacente, montrant à quel point Elm et JavaScript peuvent travailler ensemble.

## Voir Aussi 

1. Documentation Elm sur la manipulation de chaînes : http://package.elm-lang.org/packages/elm-lang/core/latest/String 
2. Tutoriel Elm : https://guide.elm-lang.org/ 
3. Outil Elm online pour tester votre code : https://elmrepl.netlify.com/