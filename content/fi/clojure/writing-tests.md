---
title:                "Testien kirjoittaminen"
html_title:           "Clojure: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

Mitä & Miksi?

Testien kirjoittaminen on prosessi, jossa kirjoitetaan koodia tarkoituksena testata ohjelmiston toimivuutta ja löytää mahdollisia virheitä ja puutteita. Tämä on tärkeä osa kehittäjien työtä varmistaakseen, että heidän koodinsa toimii halutulla tavalla ja parantaakseen sen laatua.

Miten:
```Clojure 
(ns testi.core-test
  (:require [clojure.test :refer :all]
            [testi.core :refer :all]))

(deftest test-lisaa
  (is (= 5 (lisaa 2 3)))
  (is (= 0 (lisaa -2 2)))
  (is (= 10 (lisaa 5 5))))

(deftest test-kertoo
  (is (= 15 (kertoo 3 5)))
  (is (= 0 (kertoo 0 7)))
  (is (= 12 (kertoo -3 -4))))

(run-tests)
```
*Oletetaan että `lisaa`- ja `kertoo`-funktiot on määritelty `testi.core`-nimisessä tiedostossa.*

```Clojure
Testing testi.core-test
(:test (:var testi.core-test/test-lisaa))
 testing (testi.core-test/test-lisaa)...
FAIL in () (test_core.clj:4)
expected: (= 5 (lisaa 2 3))
actual: (not (= 5 6))
FAIL in () (test_core.clj:5)
expected: (= 0 (lisaa -2 2))
actual: (not (= 0 4))
FAIL in () (test_core.clj:6)
expected: (= 10 (lisaa 5 5))
actual: (not (= 10 4))

(:test (:var testi.core-test/test-kertoo))
 testing (testi.core-test/test-kertoo)...
FAIL in () (test_core.clj:9)
expected: (= 15 (kertoo 3 5))
actual: (not (= 15 -1))
FAIL in () (test_core.clj:10)
expected: (= 0 (kertoo 0 7))
actual: (not (= 0 9))
FAIL in () (test_core.clj:11)
expected: (= 12 (kertoo -3 -4))
actual: (not (= 12 5))

Finished in 0.00052 seconds
6 examples, 6 failures
```

## Syväsukellus
Testien kirjoittaminen on tullut yhä tärkeämmäksi osaksi ohjelmistokehitystä, kun ohjelmistojen koko ja monimutkaisuus lisääntyvät. Ensimmäiset testaustyökalut otettiin käyttöön jo 1950-luvulla, mutta vasta 1990-luvulla testiautomaatio ja test-driven development eli testidrivinen kehitys tuli suosituksi. Nykyään testien kirjoittaminen on oleellinen osa luotettavan ja toimivan ohjelmiston kehitystä.

Vaikka Clojurella on vahva ja tyylikäs syntaksi, on olemassa myös muita vaihtoehtoja testien kirjoittamiseen, kuten JUnit ja TestNG Java-ohjelmointikielelle tai rspec Ruby-ohjelmointikielelle. Näistä vaihtoehdoista jokaisella on omat vahvuutensa, mutta Clojuren hyöty on sen funktionaalisen ohjelmoinnin ominaisuudet, jotka helpottavat testien kirjoittamista.

Testien kirjoittamisessa on tärkeää kattaa mahdollisimman paljon erilaisia skenaarioita ja testauskattavuus on hyvä pitää mahdollisimman korkealla. Hyvät testit auttavat kehittäjiä nopeasti paikantamaan ja korjaamaan mahdollisia virheitä sekä varmistamaan, että jokainen muutos ohjelmistossa ei riko jo olemassa olevaa toiminnallisuutta.

## Katso myös
- [Official ClojureDoc for clojure.test](https://clojuredocs.org/clojure.test)
- [Clojure-test: a Competent Testing Library for Clojure](https://medium.com/funding-circle-engineering/clojure-test-a-competent-testing-library-for-clojure-2c221e6f58df)