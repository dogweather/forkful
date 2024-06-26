---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:02.482478-07:00
description: "Hvordan: Clojure, ved \xE5 dra nytte av JVM, st\xF8tter forskjellige\
  \ testrammeverk. Imidlertid er et ofte brukt innebygd bibliotek `clojure.test`.\
  \ Her er et\u2026"
lastmod: '2024-03-13T22:44:40.405780-06:00'
model: gpt-4-0125-preview
summary: "Clojure, ved \xE5 dra nytte av JVM, st\xF8tter forskjellige testrammeverk."
title: Skrive tester
weight: 36
---

## Hvordan:
Clojure, ved å dra nytte av JVM, støtter forskjellige testrammeverk. Imidlertid er et ofte brukt innebygd bibliotek `clojure.test`. Her er et enkelt eksempel:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Addisjonsfunksjonalitet"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Etter å ha kjørt denne testen, ville du se en utskrift som ligner på:

```
Tester example.test

Kjørte 2 tester som inneholdt 2 påstander.
0 feil, 0 feil.
```

For de som søker mer funksjonsrike alternativer, kan man utnytte tredjepartsbiblioteker som `Midje` eller `test.check`. Slik kan du bruke Midje for en lignende test:

Først, legg til Midje i dine project.clj avhengigheter:
```clojure
[midje "1.9.9"]
```

Deretter kan testen din med Midje se slik ut:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Tester addisjon"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Ved å kjøre testen gjennom Midje med `lein midje`, ville utskriften vise noe liknende:

```
Alle sjekker (2) lyktes.
```
