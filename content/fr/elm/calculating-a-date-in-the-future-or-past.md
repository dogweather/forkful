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

## Qu'est-ce que c'est & Pourquoi?

Calculer une date dans le futur ou le passé est une tâche consistant à déterminer une date spécifique en se basant sur une autre date. Les programmeurs font cela pour gérer les événements déclenchés à des moments spécifiques, comme les rappels, les abonnements et les tâches planifiées.

## Comment Faire:

Voici comment vous pouvez le faire en Elm. Vous utiliserez le package `time`:

```Elm
import Time

calculerDateFutur : Time.Posix -> Time.Zone -> Int -> Time.Posix
calculerDateFutur dateActuelle zone jours =
    let
        durée = Time.inDays jours
    in
    Time.add durée dateActuelle
```

Cette fonction ajoute simplement le nombre de jours spécifié à la date actuelle pour obtenir une date future.

Et pour calculer une date dans le passé, vous pouvez simplement passer un nombre de jours négatif.

```Elm
calculerDatePassé : Time.Posix -> Time.Zone -> Int -> Time.Posix
calculerDatePassé dateActuelle zone jours =
    calculerDateFutur dateActuelle zone -jours
```

## Deep Dive:

L'idée de calculer une date spécifique dans le futur ou le passé est assez ancienne en programmation. Cependant, le faire de manière précise et fiable peut être délicat, en raison de facteurs tels que les horaires d'été, les secondes intercalaires et les différences de fuseau horaire.

Une alternative consiste à utiliser des bibliothèques ou des services externes qui traitent de ces détails pour vous. C'est aussi une option valable, surtout si vous avez besoin de gérer des choses comme les événements récurrents ou des calendriers complets.

Dans Elm, vous devez travailler avec `Time.Posix` et `Time.Zone` pour effectuer ces calculs. Notez que `Time.Zone` est spécifique au lieu dans lequel vous allez effectuer les calculs. Dans la plupart des cas, vous pouvez simplement utiliser `Time.utc` pour cela.

## Voir aussi:

Pour plus d'informations, consultez les liens suivants:

- Comment utiliser le package `time` en Elm (https://package.elm-lang.org/packages/elm/time/latest/)
- Un bon article sur l'importance de la gestion correcte du temps en programmation (https://www.zachleat.com/elm/time-to-elm)
- Une discussion détaillée sur les défis liés à la manipulation du temps en programmation (https://en.wikipedia.org/wiki/System_time)