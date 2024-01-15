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

## Pourquoi

Si vous êtes fasciné par les différentes manières de manipuler les dates, ou si vous avez besoin de calculer une date dans le futur ou le passé pour un projet, cet article est pour vous.

## Comment faire

Pour calculer une date dans le futur ou le passé en utilisant Elm, vous pouvez utiliser la fonction `Time.add` en spécifiant la valeur du temps à ajouter ou à soustraire. Voici un exemple pour calculer la date dans un an à partir d'aujourd'hui :

```Elm
import Time exposing (..)

-- Calculer la date dans un an
dateFuture = Time.add Time.years 1 Time.now

-- Afficher la date au format ISO8601
dateString = Date.toIsoString dateFuture

-- Output: 2022-09-21T12:00:00.000Z
```

Pour calculer une date dans le passé, il suffit de changer l'ordre des paramètres dans la fonction `Time.add` et de spécifier une valeur négative. Par exemple, pour calculer la date d'il y a un mois :

```Elm
import Time exposing (..)

-- Calculer la date d'il y a un mois
datePast = Time.add Time.months -1 Time.now

-- Afficher la date au format JJ/MM/AAAA
dateString = Date.fromTime datePast |> Date.toDay |> Date.toMonth |> Date.toYear
  |> toString 

-- Output: 31/08/2021
```

## Deep Dive

Lorsque vous utilisez la fonction `Time.add` pour calculer une date dans le futur ou le passé, il est important de comprendre les unités de temps disponibles pour spécifier la valeur à ajouter ou à soustraire. Voici les principales unités de temps disponibles dans la bibliothèque Elm Time :

- `Time.years`
- `Time.months`
- `Time.days`
- `Time.hours`
- `Time.minutes`
- `Time.seconds`

Vous pouvez également utiliser des combinaisons de ces unités pour calculer une date plus précise, par exemple `Time.add (Time.years 2) (Time.days 10) Time.now` pour calculer la date dans deux ans et dix jours à partir d'aujourd'hui.

## Voir aussi

- [Documentation officielle de Time dans Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Article sur la manipulation des dates en Elm (en anglais)](https://dev.to/emilybache/handling-dates-in-elm-26cl)