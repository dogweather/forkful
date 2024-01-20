---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Comparer deux dates, c'est déterminer laquelle est la plus récente. On le fait pour traiter et analyser les données de temps en programmation.

## Comment faire :

Dans Gleam, vous pouvez comparer deux dates en utilisant la fonction compare(). Voici comment :

```Gleam
let dateA = date.new(2020, 12, 1)
let dateB = date.new(2021, 12, 1)
let comparaison = date.compare(dateA, dateB)
```

Le programme va retourner -1 si dateA est plus ancienne, 0 si les dates sont identiques, et 1 si dateB est plus ancienne. 

## Exploration :

Comparer les dates a toujours été un élément essentiel de la programmation. Historiquement, elle a été utilisée pour ordonnancer des tâches, journaliser des événements et tracer des dépendances temporelles. Dans Gleam, vous pouvez également utiliser les fonctions Day.compare/2 ou NaiveDateTime.compare/2 si vous traitez respectivement avec des jours ou des horaires précis. 

Quant à la mise en œuvre, la comparaison des dates repose sur la conversion des dates en millisecondes depuis l'Epoch Unix (0h00 le 1er janvier 1970) et la comparaison de ces valeurs.

## Voir aussi :

D'autres ressources pour approfondir vos connaissances :
1. Documentation Gleam pour les dates : https://hexdocs.pm/gleam_stdlib/gleam/date/
2. Tutoriel sur la gestion des dates : https://www.tutorialspoint.com/gleam/gleam_date_time_functions.htm
3. Comparaison de dates dans d'autres langages : https://www.freecodecamp.org/news/how-to-compare-dates-with-javascript/