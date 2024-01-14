---
title:                "Clojure: Calculer une date dans le futur ou le passé"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi
Calculer une date dans le futur ou le passé peut être utile dans de nombreuses situations, notamment pour planifier des événements ou des tâches, ou pour analyser des données historiques.

## Comment Faire
Pour calculer une date dans le futur ou le passé en Clojure, vous pouvez utiliser la fonction `clojure.java-time/plus` et `clojure.java-time/minus`. Ces fonctions prennent en entrée une date et une durée, et renvoient une nouvelle date en fonction de la direction souhaitée.

```Clojure
(require '[java-time :as time])

(time/plus (time/local-date 2020 1 1) (time/period 2 :years))
;; => #object[java.time.LocalDate 2022-01-01]

(time/minus (time/local-date 2020 1 1) (time/period 1 :months))
;; => #object[java.time.LocalDate 2019-12-01]
```

Vous pouvez également utiliser la fonction `time/plus` et `time/minus` avec les unités de temps telles que les jours, les heures, les minutes et les secondes.

## Plongée Profonde
Lorsque vous calculez une date dans le futur ou le passé, il est important de prendre en compte les années bissextiles et les différents nombres de jours dans chaque mois. En Clojure, vous pouvez utiliser la fonction `time/month` pour obtenir le nombre de jours dans un mois donné.

```Clojure
(time/month (time/month "2020-02"))
;; => 29
```

De plus, vous pouvez également utiliser les fonctions `time/plus` et `time/minus` pour calculer des dates basées sur des instants précis, en utilisant la classe `java.time.Period`.

```Clojure
(time/plus (time/local-date-time 2020 1 1 12 30 0) (time/period 1 :days))
;; => #object[java.time.LocalDateTime 2020-01-02T12:30:00]

(time/minus (time/zoned-date-time 2020 1 1 12 30 0 (time/zone-id "Europe/Paris")) (time/period 2 :hours))
;; => #object[java.time.ZonedDateTime 2020-01-01T10:30:00+01:00[Europe/Paris]]
```

## Voir Aussi
- Documentation Clojure de `java-time`: https://clojure.github.io/java.time/java-time.core-api.html
- Guide officiel de Java pour la manipulation de dates et d'heures: https://docs.oracle.com/javase/tutorial/datetime/overview/index.html