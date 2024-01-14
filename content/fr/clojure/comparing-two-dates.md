---
title:                "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de comparer deux dates afin de déterminer l'ordre chronologique des événements. Cela peut être utile dans de nombreuses situations, telles que la planification, la gestion de tâches et la vérification des délais.

## Comment faire

Pour comparer deux dates en Clojure, vous pouvez utiliser la fonction `compare`. Cette fonction prend deux arguments de type `java.util.Date` et renvoie un nombre entier représentant la relation entre les deux dates. Voici un exemple de code :

```Clojure
(def date1 (java.util.Date. 2021 9 15))
(def date2 (java.util.Date. 2021 10 1))

;; Comparaison des dates
(def result (compare date1 date2))

;; Affichage du résultat
(println result)
```

Dans cet exemple, la fonction `compare` renvoie -1 car `date1` est antérieure à `date2`. Voici les valeurs renvoyées possibles par la fonction `compare` :

- -1 : si la première date est antérieure à la deuxième date
- 0 : si les deux dates sont égales
- 1 : si la première date est postérieure à la deuxième date

Vous pouvez également utiliser la fonction `before?` ou `after?` pour vérifier si une date est antérieure ou postérieure à une autre. Voici un autre exemple de code :

```Clojure
(def date3 (java.util.Date. 2021 9 15))
(def date4 (java.util.Date. 2021 10 1))

;; Vérification de la relation entre les dates
(def result (before? date3 date4))

;; Affichage du résultat
(println result)
```

Le code ci-dessus renverra `true` car `date3` est antérieure à `date4`.

## Plongée profonde

Lors de la comparaison de dates en Clojure, il est important de comprendre comment ces dates sont représentées dans le langage. En utilisant la fonction `java.util.Date`, les dates sont représentées en millisecondes depuis le 1er janvier 1970, à 00:00:00 UTC.

Il existe également d'autres librairies en Clojure, telles que `clj-time`, qui offrent des fonctionnalités avancées pour la manipulation de dates et de temps. Vous pouvez également utiliser des librairies externes telles que `joda-time` pour des fonctionnalités encore plus avancées.

## Voir aussi

- [Documentation officielle de Clojure sur la comparaison de dates](https://clojuredocs.org/clojure.core/compare)
- [Tutoriel sur la manipulation de dates en Clojure avec la librairie clj-time](https://www.baeldung.com/clojure-date-time)
- [Documentation officielle de joda-time](https://www.joda.org/joda-time/)