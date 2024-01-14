---
title:                "Clojure: Génération de nombres aléatoires"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Générer des nombres aléatoires est une tâche courante en programmation, que ce soit pour créer des jeux, des simulations ou pour tester des algorithmes. Dans cet article, nous allons expliquer comment générer des nombres aléatoires en utilisant Clojure, un langage de programmation fonctionnel dynamique.

# Comment Faire

Pour générer des nombres aléatoires en Clojure, nous allons utiliser la fonction `rand`, qui retourne un nombre aléatoire entre 0 (inclus) et 1 (exclus). Nous pouvons ensuite multiplier le résultat par un nombre pour obtenir une plage de valeurs différentes. Par exemple, pour générer un nombre aléatoire entre 1 et 10, nous pouvons utiliser `(* 10 (rand))`.

Voici un exemple pratique de génération de 5 nombres aléatoires entre 1 et 100:

```Clojure
(dotimes [i 5]
  (println "Nombre aléatoire #" (inc i) ": " (* 100 (rand))))
```

Et voici un exemple avec une liste de valeurs possibles et une fonction pour choisir un élément aléatoire de cette liste:

```Clojure
(def noms ["John" "Jane" "Bob" "Alice"])

(defn random-nom []
  (nth noms (rand-int (count noms))))

(println "Nom aléatoire: " (random-nom))
```

Ces sont des exemples basiques, mais en combinant la fonction `rand` avec d'autres fonctions de manipulation de séquences en Clojure, les possibilités sont infinies.

# Plongée Profonde

Il est important de noter que la fonction `rand` ne génère pas vraiment des nombres aléatoires, mais plutôt des nombres pseudo-aléatoires. Cela signifie que si nous fournissons la même graine (seed) à la fonction, elle retournera la même séquence de nombres aléatoires.

Pour éviter cela, nous pouvons utiliser la fonction `rand-nth`, qui prend une séquence en argument et retourne un élément aléatoire de cette séquence. En fournissant une séquence de nombres entiers, nous pouvons obtenir des nombres aléatoires sans avoir à nous soucier de la graine.

# Voir Aussi

- [Documentation officielle Clojure](https://clojure.org/)
- [Documentation officielle pour la fonction rand](https://clojuredocs.org/clojure.core/rand)
- [Vidéo sur la génération de nombres aléatoires en Clojure](https://www.youtube.com/watch?v=K-37X2TGwVo)