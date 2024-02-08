---
title:                "Suppression de caractères correspondant à un motif"
aliases:
- fr/elm/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:10.081373-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Supprimer des caractères selon un motif, c’est filtrer une chaîne de caractères pour en retirer certains éléments indésirables. Les programmeurs font cela pour nettoyer les entrées de l'utilisateur, formater des données, ou encore simplifier le traitement du texte.

## How to:
Elm utilise des "Regex" (expressions régulières) pour identifier les motifs à supprimer. Voyez l'exemple ci-dessous :

```Elm
import Regex exposing (Regex, fromString, replace)

removePattern : String -> String -> String
removePattern pattern text =
    let
        justPattern : Regex
        justPattern =
            fromString pattern |> Maybe.withDefault (Regex.fromString "" |> Maybe.withDefault Regex.never)
    in
    replace All justPattern (\_ -> "") text

-- Utilisation
cleanText : String
cleanText =
    removePattern "[0-9]" "E1lm 0is c2ool!"

{- Sortie : "Elm is cool!" -}
```

## Deep Dive
Les expressions régulières sont puissantes, utilisées depuis les premiers jours d'Unix, et adoptées dans presque tous les langages de programmation. En Elm, `Regex` est un module permettant leur utilisation. On crée une `Regex` avec `fromString`, puis on filtre avec `replace`. Attention, Elm n'offre pas toutes les fonctionnalités des Regex rencontrées dans d'autres langages — Elm privilégie la simplicité et la sûreté d'exécution. Côté alternatives, les fonctions `String` peuvent aussi servir à manipuler du texte, mais elles sont moins flexibles pour des motifs complexes.

## See Also
Pour approfondir votre compréhension des Regex dans Elm et découverte d’autres fonctions de traitement de chaînes :

- Documentation officielle du module Regex d'Elm : [Elm Regex](http://package.elm-lang.org/packages/elm/regex/latest)
- Guide pratique sur les expressions régulières : [RegexOne](https://regexone.com/)
- Documentation Elm sur la manipulation de chaînes de caractères : [Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
