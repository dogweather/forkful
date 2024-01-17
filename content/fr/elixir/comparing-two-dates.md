---
title:                "Comparer deux dates"
html_title:           "Elixir: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates est un processus couramment utilisé par les programmeurs pour déterminer la différence entre deux moments dans le temps. Cela peut être utile pour des tâches telles que la planification, la gestion de données et la résolution de bugs. En utilisant Elixir, nous pouvons facilement comparer deux dates pour obtenir des résultats précis.

## Comment faire:

```Elixir
date1 = ~D[2021-08-01]
date2 = ~D[2021-08-07]
diff = Date.diff(date1, date2)
IO.puts diff
```

La sortie de ce code sera "6", car il y a 6 jours d'écart entre le 1er août 2021 et le 7 août 2021.

Vous pouvez également comparer les heures en utilisant le même principe avec les fonctions de date et d'heure.

```Elixir
dateTime1 = ~U[2021-08-01 12:00:00.00]
dateTime2 = ~U[2021-08-02 10:00:00.00]
diff = DateTime.diff(dateTime1, dateTime2)
IO.puts diff
```

La sortie de ce code sera "22 heures et 0 minutes", car il y a 22 heures d'écart entre le 1er août 2021 à 12h00 et le 2 août 2021 à 10h00.

## Plongée en profondeur:

La comparaison de dates remonte à l'utilisation du calendrier julien et du calendrier grégorien pour suivre le temps. Ces calendriers prennent en compte les changements dans la rotation de la Terre autour du soleil, permettant un calcul précis des années bissextiles. Alternativement, certains programmeurs peuvent utiliser des bibliothèques tierces telles que Timex pour comparer les dates en Elixir.

Au-delà de la simple comparaison de dates, Elixir offre également des fonctions pour comparer les moments et les durées. Cela peut être utile lors de la planification d'événements, de l'analyse de données temporelles et d'autres tâches liées au temps.

## Voir aussi:

- Documentation officielle pour la comparaison de dates en Elixir: https://hexdocs.pm/elixir/Date.html#diff/2
- Bibliothèque Timex pour la manipulation du temps en Elixir: https://hexdocs.pm/timex/readme.html
- Article de blog "Working with Dates and Time in Elixir": https://www.poeticoding.com/working-with-dates-and-time-in-elixir/