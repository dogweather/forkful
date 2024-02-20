---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:57.989081-07:00
description: "\xC9crire des tests en Clojure, tout comme dans d'autres langages de\
  \ programmation, implique de cr\xE9er du code d\xE9di\xE9 pour v\xE9rifier que votre\
  \ code principal\u2026"
lastmod: 2024-02-19 22:05:16.184306
model: gpt-4-0125-preview
summary: "\xC9crire des tests en Clojure, tout comme dans d'autres langages de programmation,\
  \ implique de cr\xE9er du code d\xE9di\xE9 pour v\xE9rifier que votre code principal\u2026"
title: "R\xE9daction de tests"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests en Clojure, tout comme dans d'autres langages de programmation, implique de créer du code dédié pour vérifier que votre code principal fonctionne comme prévu. Cela aide à garantir la correction, à faciliter le refactoring et à améliorer la stabilité du code.

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
