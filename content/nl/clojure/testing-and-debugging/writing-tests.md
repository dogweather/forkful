---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:50.672347-07:00
description: "Tests schrijven betekent code cre\xEBren die controleert of andere code\
  \ werkt zoals verwacht. Programmeurs doen dit om bugs te vangen, betrouwbaarheid\
  \ te\u2026"
lastmod: '2024-03-11T00:14:24.234831-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven betekent code cre\xEBren die controleert of andere code\
  \ werkt zoals verwacht. Programmeurs doen dit om bugs te vangen, betrouwbaarheid\
  \ te\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?
Tests schrijven betekent code creëren die controleert of andere code werkt zoals verwacht. Programmeurs doen dit om bugs te vangen, betrouwbaarheid te verzekeren en hoofdpijn later te besparen.

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
