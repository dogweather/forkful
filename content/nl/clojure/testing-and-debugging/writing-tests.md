---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:50.672347-07:00
description: 'Hoe: Clojure gebruikt een bibliotheek genaamd `clojure.test` om tests
  te schrijven en uit te voeren. Hier is hoe je het gebruikt.'
lastmod: '2024-03-13T22:44:50.423190-06:00'
model: gpt-4-0125-preview
summary: Clojure gebruikt een bibliotheek genaamd `clojure.test` om tests te schrijven
  en uit te voeren.
title: Tests Schrijven
weight: 36
---

## Hoe:
Clojure gebruikt een bibliotheek genaamd `clojure.test` om tests te schrijven en uit te voeren. Hier is hoe je het gebruikt:

```Clojure
(require '[clojure.test :refer :all])

(deftest addition-test
  (testing "Basis optelling"
    (is (= 4 (+ 2 2)))))
    
(run-tests)
```

Voorbeelduitvoer na het uitvoeren van de test:

```
lein test gebruiker
Testen gebruiker

1 tests uitgevoerd, met 1 beweringen.
0 fouten, 0 fouten.
```

## Diepgaande duik
Clojure's testaanpak komt voort uit de REPL-gedreven ontwikkelomgeving. Generatief testen met `test.check` en eigenschapsgebaseerd testen zijn alternatieve strategieën. Ze genereren automatisch testgevallen in plaats van alles met de hand te schrijven. Implementatie leunt sterk op macro's, wat zorgt voor een dynamische testomgeving.

## Zie ook
- [Clojure Testing](https://clojure.org/gidsen/deps_and_cli#_testen)
- [clojure.test documentatie op GitHub](https://github.com/clojure/clojure/blob/master/src/clj/clojure/test.clj)
- [Inleiding tot eigenschapsgebaseerd testen met `test.check`](https://github.com/clojure/test.check)
