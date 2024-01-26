---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests, c'est vérifier que notre code fait ce qu'il doit. Les développeurs font cela pour éviter les bugs, gagner du temps au long terme, et pour que le refactoring ne devienne pas un cauchemar.

## Comment ?

Pour faire un test en Clojure, on utilise souvent la librairie `clojure.test`. Voici un exemple :

```Clojure
(require '[clojure.test :refer :all])

(deftest addition-test
  (testing "Testons l'addition simple"
    (is (= 4 (+ 2 2)))))
    
(run-tests)

```

La sortie ressemblera à quelque chose comme ça :

```
Testing user

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Exploration profonde
Historiquement, les tests en programmation ont pris de l'ampleur avec l'arrivée des méthodologies agiles. Alternativement, des frameworks comme `Midje` et `expectations.clojure.test` existent pour ceux qui cherchent autre chose. Concernant l'implantation, Clojure, étant fonctionnelle, favorise les tests unitaires et l'approche TDD, où les fonctions pures sont plus faciles à tester du fait de leur absence d'effets de bord.

## Voir aussi
- [clojure.test](https://clojure.github.io/clojure/clojure.test-api.html) pour la documentation officielle.
- [Clojure for the Brave and True](https://www.braveclojure.com/) pour un guide sur Clojure, incluant des parties sur le test.
- [Midje sur GitHub](https://github.com/marick/Midje) pour une alternative de clojure.test.
