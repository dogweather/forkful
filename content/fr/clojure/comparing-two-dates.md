---
title:    "Clojure: Comparer deux dates"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

La comparaison de deux dates est une tâche courante en programmation. Cela peut être utile pour vérifier si une date est antérieure, postérieure ou égale à une autre date, ou pour effectuer des calculs basés sur la différence entre deux dates. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Clojure.

# Comment Faire

Pour comparer deux dates en utilisant Clojure, nous allons utiliser la fonction `compare` de la bibliothèque `java.util.Date`. Cette fonction prend deux dates en argument et renvoie un entier qui indique si la première date est antérieure, égale ou postérieure à la seconde date.

```
Clojure
(let [date1 (java.util.Date. 2021 3 1)
      date2 (java.util.Date. 2021 4 1)]

  (println (compare date1 date2)))

;; Output: -1
```

Dans cet exemple, nous déclarons deux variables `date1` et `date2` qui contiennent deux dates différentes. Ensuite, nous utilisons la fonction `compare` pour comparer ces deux dates et affichons le résultat, qui est -1 car la première date est antérieure à la seconde.

Nous pouvons également utiliser la fonction `before?` ou `after?` pour vérifier si une date est respectivement antérieure ou postérieure à une autre date.

```
Clojure
(let [date1 (java.util.Date. 2021 3 1)
      date2 (java.util.Date. 2021 4 1)]

  (println (before? date1 date2)))

;; Output: true

(let [date1 (java.util.Date. 2021 3 1)
      date2 (java.util.Date. 2021 4 1)]

  (println (after? date1 date2)))

;; Output: false
```

# Plongée Profonde

La compréhension du temps et des dates peut être un sujet complexe en programmation. En utilisant Clojure, nous pouvons également manipuler les dates en utilisant la bibliothèque `clj-time`. Cette bibliothèque offre des fonctions plus avancées pour travailler avec les dates, telles que la conversion de chaînes en date et la comparaison de dates avec une précision allant jusqu'à la minute.

Il est également important de se rappeler que la comparaison de dates peut être sensible au fuseau horaire. Il est recommandé d'utiliser les fonctions de la bibliothèque `clj-time` pour gérer les fuseaux horaires de manière plus précise.

# Voir Aussi

- Documentation officielle de Clojure sur la fonction `compare`: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/compare
- Documentation officielle de la bibliothèque `clj-time`: https://github.com/clj-time/clj-time
- Tutoriel sur le travail avec les dates en Clojure avec `clj-time`: https://www.braveclojure.com/core-functions-in-depth/#clj-time