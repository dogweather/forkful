---
title:                "Génération de nombres aléatoires."
html_title:           "Clojure: Génération de nombres aléatoires."
simple_title:         "Génération de nombres aléatoires."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Aléatoire : Pourquoi et Comment?

## Quoi & Pourquoi?
Générer des nombres aléatoires est un processus informatique qui consiste à produire des nombres aléatoires de manière pseudo-aléatoire. Cela permet aux programmeurs de créer des jeux, des simulations et des outils statistiques qui nécessitent un élément de hasard.

## Comment faire:
Pour générer des nombres aléatoires en Clojure, on peut utiliser la fonction `rand` qui retourne un nombre aléatoire compris entre 0 inclus et 1 exclus. Par exemple, pour générer un nombre aléatoire entre 1 et 100, on peut utiliser l'expression `(+ 1 (int (* 100 (rand))))` qui multiplie le nombre aléatoire par 100 et l'arrondit à l'entier le plus proche avant d'ajouter 1. Voici un exemple de code et sa sortie :

```Clojure
(defn randomize [min max]
  (+ min (int (* max (rand)))))

(randomize 1 10)   ; Exemple de sortie : 7
```

## Plongeon En Profondeur:
Historiquement, la génération de nombres aléatoires était un problème difficile en informatique. Les premières méthodes utilisaient des formules mathématiques mais produisaient des résultats prévisibles. Avec l'avènement de l'informatique moderne, les ordinateurs utilisent des algorithmes complexes pour produire des nombres pseudo-aléatoires. Il existe également des alternatives à la fonction `rand` en Clojure, telles que `random-int` qui accepte une limite supérieure et retourne un entier aléatoire compris entre 0 et cette limite.

## A Voir Aussi:
Pour en savoir plus sur la génération de nombres aléatoires en Clojure, vous pouvez consulter la documentation officielle à ce sujet : https://clojuredocs.org/clojure.core/rand. Vous pouvez également explorer les différentes alternatives mentionnées ci-dessus telles que `random-int` et `random-sample` pour découvrir leurs fonctionnalités respectives.