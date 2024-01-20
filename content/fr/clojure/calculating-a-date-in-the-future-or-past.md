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

## Quoi & Pourquoi?
Les calculs de date future ou passée correspondent à la détermination d'une date relative à une autre. Les développeurs font cela pour diverses raisons, surtout pour planifier des événements ou estimer des délais.

## Comment Faire:
En Clojure, nous utilisons les fonctions `plus` et `minus` du paquet `clj-time`. Voici un exemple de calcul des dates:

```Clojure
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c]
         '[clj-time.periodic :as p])

(def date-actuelle (t/now))
(def date-future (t/plus date-actuelle (t/days 10))) ;; machine à temps, allons 10 jours en avant 
(def date-passee (t/minus date-actuelle (t/days 5))) ;; et 5 jours en arrière

;;output
(date-actuelle)
"2022-08-24T06:20:12.684Z"

(date-future)
"2022-09-03T06:20:12.684Z"

(date-passee)
"2022-08-19T06:20:12.684Z"
```

## Approfondissement
Les dates futures et passées sont un concept qui remonte à la nuit des temps où les humains ont cherché à mesurer le temps pour pouvoir prédire et planifier. Alternativement, vous pouvez utiliser `java.util.Calendar` ou `java.time.ZonedDateTime`, mais clj-time reste la solution la plus élégante pour Clojure.

Il est important de noter que clj-time repose sur Joda-Time, une bibliothèque de gestion du temps pour Java, pour ses opérations de base. Mais il offre aussi des fonctionnalités supplémentaires telle que la manipulation facile des fuseaux horaires, les périodes, les durées, etc.

## Voir aussi
Pour en savoir plus sur la programmation des dates en Clojure, consultez les lien suivants : 

1. Documentation clj-time : https://github.com/clj-time/clj-time
2. Joda-Time: http://www.joda.org/joda-time/