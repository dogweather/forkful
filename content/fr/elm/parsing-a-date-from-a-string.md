---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing d'une date à partir d'une chaîne de caractères en Elm

## Qu'est-ce que c'est & Pourquoi?

Le "parsing" d'une date à partir d'une chaîne de caractère est la conversion d'un format textuel dans une date "réelle" et utile. Les développeurs font cela pour travailler avec les dates dans des calculs, des filtrages et des affichages plus précis.

## Comment faire:

Voici un exemple simple d'implémentation de parsing d'une date à partir d'une chaîne en Elm:

```Elm
import Time

type alias DateString = {
  year : String
  month: String
  day  : String
}

parseDate : DateString -> Time.Posix
parseDate {year, month, day} = 
  let
    y = String.toInt year
    m = String.toInt month
    d = String.toInt day
  in
  Time.fromCalendarDate y m d
```

Dans cet exemple, un objet `DateString` est converti en type `Time.Posix`, utilisable pour des opérations de date.

## Approfondissement

Historiquement, le parsing d'une date à partir d'une chaîne de caractères était plus compliqué, notamment à cause des formats de date différents en fonction des régions du monde. Par exemple, `02/03/2021` peut représenter le 2 mars 2021 ou le 3 février 2021. Elm évite ces problèmes en utilisant le format ISO 8601.

Dans Elm, il est possible d'utiliser d'autres bibliothèques pour le parsing de date, telle 'elm/iso8601-date-strings'. Parfois, le parsing personnalisé est même nécessaire, en fonction de vos besoins de format.

La fonction `Time.fromCalendarDate` convertit un ensemble d'entiers en un type Time.Posix. Ce processus inclut la vérification de validité de la date, donc si vous passez une date non valide, le programme peut déclencher une erreur à l'exécution.

## Voir aussi

Pour plus d'informations sur le travail avec les dates et le temps en Elm, vous pouvez consulter:

1. [La documentation officielle Elm sur le module Time](https://package.elm-lang.org/packages/elm/time/latest/Time)