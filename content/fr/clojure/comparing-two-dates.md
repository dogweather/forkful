---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Comparer deux dates c'est évaluer leurs différences suivant un certain critère. C'est une opération récurrente en programmation, utilisée pour résoudre des problèmes liés à la gestion du temps comme établir des durées ou déterminer l’ordre chronologique d’événements.

## Comment Faire :
Premièrement, ajoutons la bibliothèque clojure.java-time pour gérer les dates.

```clojure
(:require [java-time :as jt])
```

Créons deux dates pour la comparaison :

```clojure
(def date1 (jt/local-date 2022 3 15))
(def date2 (jt/local-date 2023 6 19))
```

Ensuite, utilisons la fonction `before?` pour voir si une date est avant une autre date :

```clojure
(jt/before? date1 date2)
```

Output :

```clojure
true
```

Et la fonction `after?` pour voir si une date est après une autre date:

```clojure
(jt/after? date1 date2)
```

Output :

```clojure
false
```

## Plongée en Profondeur :

La bibliothèque clojure.java-time est basée sur le standard JSR-310, qui est né de l'insatisfaction des développeurs avec les bibliothèques de date/heure précédentes de Java. Il offre des classes immuables et des méthodes pour manipuler les dates et les heures qui sont beaucoup plus intuitives que celles de java.util.Date ou java.util.Calendar.

Des alternatives à l'utilisation systématique de clojure.java-time sont clj-time (une simple couche de wrapping sur Joda-Time) ou directement l'utilisation des classes LocalDate et LocalDateTime de Java.

Lors de l'utilisation de `before?` ou `after?` en Clojure pour comparer les dates, gardez à l’esprit qu’un faux peut signifier que les dates sont égales ou que l’opposé est vrai. Pour vérifier l’égalité, utilisez `equal?`.

## Voir Aussi :

Pour plus de détails sur la manipulation de dates en Clojure :
- [clojure.java-time](https://cljdoc.org/d/java-time/java-time/0.3.2/api/java-time)
- [Clojure Cookbook: Manipulating Dates and Times](https://clojure-cookbook.com/recipes/03_dates_and_times/)
- [Java SE 8 Date and Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [clojure/clj-time](https://github.com/clj-time/clj-time)