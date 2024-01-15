---
title:                "Génération de nombres aléatoires"
html_title:           "Clojure: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est utile dans de nombreuses situations en programmation. Cela peut être utilisé pour générer des données aléatoires pour des tests, créer des jeux ou même pour des problèmes d'optimisation.

## Comment faire

```Clojure
;; Générer un nombre aléatoire entre 0 et 10 inclus
(rand-int 11) 
;; -> 7

;; Générer une liste de nombres aléatoires entre 1 et 100
(repeatedly 10 #(rand-int 101)) 
;; -> (86 47 76 11 53 34 65 45 78 82)
```

## Profonde plongée

Clojure utilise la fonction `rand` pour générer des nombres aléatoires entre 0 et 1, mais il existe également des fonctions spécialisées pour générer des nombres entiers aléatoires (`rand-int`), des nombres décimaux (`rand-double`) et même des nombres dans un certain intervalle (`rand-nth`).

Il est important de noter que les nombres aléatoires générés par Clojure sont pseudo-aléatoires, ce qui signifie qu'ils suivent un algorithme déterministe mais produisent des résultats qui semblent aléatoires pour l'utilisateur. Cela garantit la reproductibilité des résultats en utilisant la même graine (`seed`) pour la fonction `rand` ou `rand-int`.

## Voir aussi

- Documentation officielle sur les fonctions de génération de nombres aléatoires en Clojure: https://clojuredocs.org/clojure.core/rand-int
- Un guide pour la génération de nombres aléatoires en Clojure: https://github.com/pedrorgirardi/clojure-random-numbers-guide