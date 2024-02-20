---
date: 2024-01-20 17:30:44.790655-07:00
description: "Calculer une date dans le futur ou le pass\xE9, c'est simplement ajouter\
  \ ou soustraire du temps \xE0 une date donn\xE9e. Les devs s'en servent pour des\
  \ t\xE2ches\u2026"
lastmod: 2024-02-19 22:05:16.456489
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9, c'est simplement ajouter\
  \ ou soustraire du temps \xE0 une date donn\xE9e. Les devs s'en servent pour des\
  \ t\xE2ches\u2026"
title: "Calcul d'une date future ou pass\xE9e"
---

{{< edit_this_page >}}

## What & Why?
Calculer une date dans le futur ou le passé, c'est simplement ajouter ou soustraire du temps à une date donnée. Les devs s'en servent pour des tâches telles que des rappels, des échéanciers ou des historiques d'événements.

## How to:
Elm utilise le paquet `elm/time` pour gérer les dates. Voici comment on calcule une date dans une semaine:

```Elm
import Time exposing (Posix)
import Time.Extra exposing (add)

calculateFutureDate : Posix -> Posix
calculateFutureDate date =
    add (7 * 24 * 60 * 60 * 1000) date  -- Ajoute 7 jours en millisecondes

-- Usage
currentDate : Posix
currentDate = Time.millisToPosix 1615194000000  -- 8 mars 2021 à 00:00:00

futureDate : Posix
futureDate = calculateFutureDate currentDate
-- futureDate est maintenant le 15 mars 2021 à 00:00:00
```

Pour une date dans le passé, soustrayez le temps:

```Elm
calculatePastDate : Posix -> Posix
calculatePastDate date =
    add (-7 * 24 * 60 * 60 * 1000) date  -- Soustrait 7 jours

-- Usage
pastDate : Posix
pastDate = calculatePastDate currentDate
-- pastDate est maintenant le 1 mars 2021 à 00:00:00
```

## Deep Dive
Historiquement, manipuler des dates en programmation est un challenge, cause des différents formats et fuseaux horaires. Elm simplifie ceci avec `elm/time` qui utilise `Posix`, représentant le temps en millisecondes depuis l'UTC 1970-01-01 00:00:00. Cela évite beaucoup de problèmes classiques de fuseaux horaires.

Niveau alternatives, y'a `elm-community/elm-time` mais `elm/time` est plus commun et robuste. On pourrait aussi tomber dans les calculs manuels, mais pourquoi se compliquer la vie ?

Pour l'implémentation, Elm traite les données de date et d'heure comme des données immuables. Donc, `add` crée une nouvelle instance `Posix` plutôt que de modifier l'original, c'est une nuance importante pour la programmation fonctionnelle.

## See Also
- Documentation officielle d'Elm sur le temps: [package.elm-lang.org/packages/elm/time/latest](https://package.elm-lang.org/packages/elm/time/latest)
- FAQ sur `elm/time`: [discourse.elm-lang.org](https://discourse.elm-lang.org/)
- Un super tutoriel sur les bases de Elm: [guide.elm-lang.org](https://guide.elm-lang.org/)
