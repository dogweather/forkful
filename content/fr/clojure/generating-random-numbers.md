---
title:    "Clojure: Génération de nombres aléatoires."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi devriez-vous vous intéresser à la génération de nombres aléatoires en programmation Clojure ? Il y a plusieurs raisons ! La génération de nombres aléatoires peut être utile pour simuler des jeux, effectuer des tests aléatoires ou générer des données pour des expériences statistiques.

## Comment faire
Pour générer des nombres aléatoires en Clojure, vous pouvez utiliser la fonction `rand`. Elle prend en argument soit un nombre entier, qui sera le maximum du nombre aléatoire généré, soit un intervalle de nombres représenté par une liste `[min max]`.

```Clojure
(+ (rand 10) 1) ; Renvoie un nombre aléatoire entre 1 et 10 inclus
(vec (repeatedly 5 #(rand-nth ["a" "b" "c"]))) ; Renvoie une liste de 5 éléments aléatoires parmi "a", "b" et "c"
```

Vous pouvez également utiliser la bibliothèque `math.combinatorics` pour générer des permutations aléatoires de listes ou de séquences.

```Clojure
require '[clojure.math.combinatorics :as combo]
(combo/permutations [:a :b :c]) ; Renvoie toutes les permutations possibles de :a, :b et :c
(shuffle [:a :b :c]) ; Mélange aléatoirement une liste ou une séquence
```

## Plongée en profondeur
Il est important de noter que la fonction `rand` utilise un générateur de nombres pseudo-aléatoires, c'est-à-dire qu'elle utilise une formule mathématique pour générer des nombres qui semblent aléatoires mais qui sont en réalité déterminés par une graine initiale. Cela signifie que si vous utilisez la même graine, vous obtiendrez toujours la même séquence de nombres aléatoires. Cela peut être utile pour reproduire des résultats, mais si vous avez besoin de réelles valeurs aléatoires, vous devriez utiliser la bibliothèque `clojure.core/rand-int` ou une bibliothèque externe comme `clojure.math.probability` qui utilise des sources externes pour générer des nombres aléatoires.

## Voir aussi
- [Documentation Clojure pour la fonction `rand`](https://clojuredocs.org/clojure.core/rand)
- [Documentation Clojure pour la bibliothèque `math.combinatorics`](https://clojuredocs.org/clojure.math.combinatorics)
- [Documentation Clojure pour la bibliothèque `clojure.math.probability`](https://clojuredocs.org/clojure.math.probability)