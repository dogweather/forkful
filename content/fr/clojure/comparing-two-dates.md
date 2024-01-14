---
title:    "Clojure: Comparaison de deux dates"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez sur un projet de développement Clojure, il peut arriver que vous ayez besoin de comparer deux dates. Cela peut sembler simple, mais il y a plusieurs façons d'approcher cette tâche et il est important de comprendre comment le faire correctement.

## Comment faire

Pour comparer deux dates en Clojure, vous devrez utiliser la fonction `compare`. Cette fonction prend deux arguments de type `java.util.Date` et renvoie -1 si la première date est antérieure à la deuxième, 1 si elle est postérieure, ou 0 si les deux dates sont égales.

```
(def today (java.util.Date.))
(def future (java.util.Date. 2021 1 1))

(compare today future)
```

La sortie de cet exemple serait 1, car le premier argument (aujourd'hui) est postérieur au deuxième (1er janvier 2021). Si vous voulez comparer deux dates spécifiques plutôt que de créer des objets `Date` à la volée, vous pouvez utiliser la fonction `date` pour les formater à partir de valeurs numériques.

```
(def jan2021 (date 2021 1 1))
(def dec2020 (date 2020 12 1))

(compare jan2021 dec2020)
```

Dans cet exemple, la sortie serait également 1, car janvier 2021 est après décembre 2020.

## Deep Dive

Lorsque vous utilisez la fonction `compare`, il est important de noter que si les deux dates ont également une valeur de temps (par exemple, 1er janvier 2021 à 12h) la fonction prendra également en compte cette information. Ainsi, si vous voulez simplement comparer les dates sans tenir compte du temps, il est conseillé de définir les valeurs de temps à 0 avant d'utiliser la fonction `compare`.

```
(def today (date 2021 1 1 12 30 0))
(def future (date 2021 1 1 0 0 0))

(compare today future)
```

Dans cet exemple, la sortie serait à nouveau 1, car le temps a été pris en compte. Mais si vous définissez les valeurs de temps à 0 pour les deux dates, la sortie serait maintenant 0, car les deux dates sont considérées comme égales.

## Voir aussi

- La documentation officielle de Clojure sur la fonction `compare`: https://clojuredocs.org/clojure.core/compare
- Un article intéressant sur les dates et le temps en Clojure: https://clojureverse.org/t/managing-dates-and-times-in-clojure/2702