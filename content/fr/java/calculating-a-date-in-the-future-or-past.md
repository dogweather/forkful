---
title:                "Calculer une date dans le futur ou le passé."
html_title:           "Java: Calculer une date dans le futur ou le passé."
simple_title:         "Calculer une date dans le futur ou le passé."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de calculer une date dans le futur ou dans le passé lorsqu'on travaille avec des programmations impliquant des utilisateurs et des événements dans le temps. Cela peut être utile pour créer des rappels, des deadlines, des prévisions, ou simplement pour afficher la date correcte dans une application.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en Java, il existe plusieurs méthodes à utiliser en fonction de vos besoins :

```Java
// Pour calculer une date dans le futur, utilisez la méthode LocalDate.now().plusDays() en indiquant le nombre de jours à ajouter
LocalDate futureDate = LocalDate.now().plusDays(numberOfDays);

// Pour calculer une date dans le passé, utilisez la méthode LocalDate.now().minusDays() en indiquant le nombre de jours à soustraire
LocalDate pastDate = LocalDate.now().minusDays(numberOfDays);

// Vous pouvez également utiliser les méthodes plusMonths(), plusYears(), minusMonths(), minusYears() pour calculer une date en fonction du mois ou de l'année
```
<sup>Exemple de sortie :</sup>

|    Input   |         Output         |
|:----------:|:----------------------:|
| 10 jours   | 2021-08-09 (pour la date actuelle de 2021-07-30) |
| 2 mois     | 2021-09-30 (pour la date actuelle de 2021-07-30) |
| 3 années   | 2024-07-30 (pour la date actuelle de 2021-07-30) |

## Plongée en profondeur

Dans Java, les dates sont représentées par la classe `LocalDate` et peuvent être manipulées grâce à différentes méthodes comme indiqué précédemment. Il existe également d'autres classes telles que `LocalDateTime` et `ZonedDateTime` qui permettent de gérer à la fois la date et l'heure.

Il est important de noter que toutes ces classes sont immuables, c'est-à-dire que leurs valeurs ne peuvent pas être modifiées une fois créées. Cela garantit la stabilité et la fiabilité des calculs de dates.

De plus, il est également possible de spécifier une zone horaire pour une date donnée afin de s'adapter aux fuseaux horaires des utilisateurs. Cela peut être fait en utilisant la méthode `atZone(ZoneId.of("votre_zone_horaire"))`.

## Voir aussi

- La documentation officielle de Java sur la classe `LocalDate` : https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Tutoriel sur la manipulation des dates en Java : https://www.baeldung.com/java-8-date-time-intro