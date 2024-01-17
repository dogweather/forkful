---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Elm: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi? 

Calculer une date dans le futur ou dans le passé est couramment utilisé par les programmeurs pour effectuer des tâches telles que la planification d'événements ou la gestion de tâches. Cela implique de manipuler et de calculer des dates en utilisant des fonctions et des formules spécifiques pour trouver la date souhaitée.

## Comment faire: 

Voici un exemple de code en Elm pour calculer une date dans le futur à partir de la date actuelle: 

```Elm
import Date exposing (..)

dateActuelle =
    fromTime 1615538900 -- 12 mars 2021 à 14h35 

dateFutur =
    add (month 5) dateActuelle  -- 12 août 2021 à 14h35 
```

La sortie de ce code sera la date du 12 août 2021 à 14h35. 

## Investigation en profondeur: 

En plus d'être utilisé dans la programmation, le calcul de dates est également un concept mathématique important qui remonte à l'époque de l'Égypte antique. Il existe également différentes approches pour calculer les dates, telles que l'utilisation de fonctions ou la manipulation de la représentation interne des dates dans un langage de programmation. Cependant, les fonctions de date intégrées dans les langages de programmation modernes permettent de faciliter grandement cette tâche.

## Voir aussi: 

- Documentation officielle d'Elm sur la manipulation de dates: https://package.elm-lang.org/packages/elm/time/latest/ 
- Un article sur l'histoire du calcul de dates: https://www.nationalgeographic.org/media/history-timekeeping/