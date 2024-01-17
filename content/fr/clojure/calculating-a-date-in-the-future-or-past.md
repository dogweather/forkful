---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Clojure: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & pourquoi?

Calculer une date dans le futur ou le passé consiste à trouver une date spécifique à un certain nombre de jours, semaines ou mois à partir d'une date donnée. Les programmeurs le font pour des raisons pratiques telles que la planification de tâches, la gestion du temps ou la manipulation de données chronologiques.

## Comment faire:

Voici une façon simple de calculer une date dans le futur ou le passé en utilisant Clojure:

```
;Exemple pour ajouter 30 jours à une date:
(def ma-date (java.util.Date.)) ;date actuelle
(def nb-jours 30)
(.add ma-date java.util.Calendar/DATE nb-jours)

;Output : La date dans 30 jours à partir de maintenant
```

```
;Exemple pour soustraire 2 semaines à une date:
(def ma-date (java.util.Date. "2021-05-01")) ;1er mai 2021
(def nb-semaines -2)
(.add ma-date java.util.Calendar/WEEK_OF_YEAR nb-semaines)

;Output : La date 2 semaines avant le 1er mai 2021
```

## Approfondissement:

- Contexte historique: La manipulation de dates est une tâche courante depuis les premiers jours de l'informatique et a donné lieu à de nombreuses méthodes différentes. Aujourd'hui, l'utilisation de bibliothèques telles que Clojure facilite grandement cette tâche pour les programmeurs.

- Alternatives: En plus de Clojure, il existe d'autres langages de programmation pouvant être utilisés pour calculer une date dans le futur ou le passé, tels que Java, Python ou JavaScript.

- Détails d'implémentation: Pour calculer une date à partir d'une date donnée, nous utilisons les méthodes de la classe Java Calendar, qui permettent d'ajouter ou de soustraire un certain nombre d'unités de temps (jours, semaines, mois) à une date donnée. Il est également possible d'utiliser la bibliothèque clojure.java-time pour une manipulation plus facile des dates et des temps en Clojure.

## A voir également:

- Pour en savoir plus sur la manipulation des dates en Clojure, consultez la documentation officielle: https://clojure.org/reference/java_interop#Calendar. 

- Vous pouvez également découvrir la bibliothèque clojure.java-time ici: https://github.com/dm3/clojure.java-time.