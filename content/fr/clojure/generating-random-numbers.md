---
title:    "Clojure: Génération de nombres aléatoires"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est une tâche courante lors de la programmation en Clojure. Cela peut être utile pour des simulations, des jeux, ou pour tout autre scénario où des valeurs aléatoires sont nécessaires.

## Comment faire

Générer des nombres aléatoires en Clojure est assez simple. Utilisez la fonction `rand` pour obtenir un nombre décimal aléatoire entre 0 inclus et 1 exclus.

```Clojure
(rand) ; Output: 0.708524928
```

Pour obtenir un nombre entier aléatoire, multipliez le résultat de `rand` par la limite supérieure voulue et utilisez ensuite la fonction `int` pour arrondir à un nombre entier.

```Clojure
(int (* (rand) 10)) ; Output: 6 ou 3 ou 9 ...
```

Vous pouvez également utiliser le `+` pour ajouter un décalage à la limite inférieure. Par exemple, pour obtenir un nombre entier aléatoire entre 5 inclus et 10 inclus :

```Clojure
(+ 5 (int (* (rand) 6))) ; Output: 6 ou 9 ou 10 ...
```

## Plongée en profondeur

Il est important de noter que la fonction `rand` utilise le générateur de nombres pseudo-aléatoires de Java par défaut. Cela signifie que si vous initialisez le générateur avec une même graine (seed), vous obtiendrez les mêmes séquences d'aléatoire à chaque fois.

```Clojure
(require '[clojure.java.api :as java])
(java/random-set-state (java/util/Random. 42)) ; Initialise le générateur avec la graine 42
(rand) ; Output: 0.03962344
```

Si vous souhaitez générer des nombres aléatoires qui sont réellement imprévisibles, utilisez la fonction `secure-random` du namespace `clojure.core`. Cela utilisera alors un générateur cryptographique plus fiable.

```Clojure
(require '[clojure.core :as core])
(core/secure-random) ; Output: 0.8862011811106673
```

## Voir aussi

- [Documentation sur la génération de nombres aléatoires en Clojure](https://clojure.org/reference/java_interop#_random_number_generation)
- [Article sur les générateurs de nombres pseudo-aléatoires en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)