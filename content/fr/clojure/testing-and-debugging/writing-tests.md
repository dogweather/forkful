---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:57.989081-07:00
description: "Comment faire : Clojure, en tirant parti de la JVM, prend en charge\
  \ divers frameworks de test. Toutefois, une biblioth\xE8que int\xE9gr\xE9e couramment\
  \ utilis\xE9e\u2026"
lastmod: '2024-03-13T22:44:57.286023-06:00'
model: gpt-4-0125-preview
summary: Clojure, en tirant parti de la JVM, prend en charge divers frameworks de
  test.
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Clojure, en tirant parti de la JVM, prend en charge divers frameworks de test. Toutefois, une bibliothèque intégrée couramment utilisée est `clojure.test`. Voici un exemple simple :

```clojure
(ns exemple.test
  (:require [clojure.test :refer :all]
            [exemple.core :refer :all]))

(deftest test-addition
  (testing "Fonctionnalité d'addition"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Après avoir exécuté ce test, vous verriez une sortie similaire à :

```
Testing exemple.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

Pour ceux qui recherchent des options plus riches en fonctionnalités, on peut utiliser des bibliothèques tierces comme `Midje` ou `test.check`. Voici comment vous pourriez utiliser Midje pour un test similaire :

D'abord, ajoutez Midje à vos dépendances de project.clj :
```clojure
[midje "1.9.9"]
```

Ensuite, votre test avec Midje pourrait ressembler à ceci :

```clojure
(ns exemple.test
  (:require [midje.sweet :refer :all]
            [exemple.core :refer :all]))

(fact "Test de l'addition"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Après l'exécution du test via Midje avec `lein midje`, la sortie afficherait quelque chose d'analogue à :

```
Tous les contrôles (2) ont réussi.
```
