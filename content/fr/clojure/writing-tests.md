---
title:                "Clojure: Écriture des tests"
simple_title:         "Écriture des tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Les tests sont une partie importante du processus de développement d'un logiciel. Ils permettent de s'assurer que le code fonctionne correctement et de détecter les bugs avant de déployer une nouvelle version. Cela peut également aider à maintenir le code propre et à faciliter la collaboration entre les développeurs.

## Comment faire

```Clojure
(defn somme [a b]
  (+ a b))
(somme 2 3)

;; Output: 5
```

Pour écrire des tests en Clojure, il est recommandé d'utiliser le framework de test intégré appelé "clojure.test". Il permet de créer des fonctions de test qui utilisent des assertions pour vérifier que les résultats obtenus sont conformes aux résultats attendus.

```Clojure
require '[clojure.test :refer :all]
(deftest somme-test
  (is (= 5 (somme 2 3))))
(run-tests)

;; Output: 
Testing user
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

La fonction "deftest" définit un test en tant que fonction et la fonction "is" définit une assertion. Ensuite, en exécutant la fonction "run-tests", nous pouvons voir le résultat des tests dans la console.

## Plongée en profondeur

Il est important d'écrire des tests pour chaque fonction que nous créons, ainsi que pour les scénarios de test qui couvrent toutes les possibilités. Cela permettra de détecter rapidement les erreurs et de s'assurer de la fiabilité du code.

Il existe également d'autres outils de test en dehors de "clojure.test", tels que "speclj" et "Midje". Ceux-ci peuvent offrir plus de fonctionnalités et de flexibilité en matière de test.

## Voir aussi

- [Clojure Docs : Testing](https://clojure-doc.org/articles/testing.html)
- [Clojure Guide : Writing Tests](https://clojure.org/guides/testing)
- [Speclj : A Testing Framework for Clojure](https://github.com/slagyr/speclj)
- [Midje : A Test Framework for Clojure and ClojureScript](https://github.com/marick/midje)