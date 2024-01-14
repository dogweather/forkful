---
title:                "Clojure: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä ja välttämätön osa ohjelmistokehitystä. Se auttaa varmistamaan koodin toimivuuden ja parantaa sen luotettavuutta ja laadukkuutta. Lisäksi se helpottaa tulevien muutosten tekemistä, sillä testien avulla voidaan nopeasti havaita mahdolliset virheet ja korjata ne ennen kuin ne aiheuttavat suurempia ongelmia.

## Kuinka

Testien kirjoittaminen Clojure-kielellä on helppoa ja sujuvaa. Seuraavassa on muutamia esimerkkejä ja tulostuksia testien kirjoittamisesta käyttämällä ```Clojure ...``` lohkoja:

```
(ns tests.core-test
  (:require [clojure.test :refer :all]
            [tests.core :refer :all]))

(deftest addition-test
  (testing "Addition should return correct result"
    (are [a b expected] (= expected (addition a b))
      5 2 7
      0 0 0))))

;; Tulostus:
;; Testing addition-test
;; Ran 2 tests containing 2 assertions.
;; 0 failures, 0 errors.
```

```
(ns tests.core-test
  (:require [clojure.test :refer :all]
            [tests.core :refer :all]))

(deftest division-test
  (testing "Division should return correct result"
    (are [a b expected] (= expected (division a b))
      10 2 5
      9 3 3
      20 0 nil))))

;; Tulostus:
;; Testing division-test
;; Ran 3 tests containing 3 assertions.
;; 0 failures, 0 errors.
```

## Syvällinen sukellus

Testien kirjoittaminen Clojurella noudattaa yleistä ohjelmointilogiikkaa. Ensimmäinen vaihe on määritellä testitapaukset, jotka tarkistavat kunkin funktion eri syötteillä. Tämän jälkeen käytetään ```are``` lohkoa, joka testaa syötteiden ja odotetun tuloksen välisen yhtäläisyyden.

On myös tärkeää muistaa testata myös virheellisillä syötteillä, jotta koodi kestää mahdollisimman monenlaisia tilanteita. Tämä voidaan tehdä esimerkiksi käyttämällä ```defspec``` tai ```s/check``` - toimintoja Speclib-kirjastosta.

Lopuksi, on tärkeää muistaa suorittaa testit ennen kuin siirrytään tuotantokoodin tekemiseen. Tämä varmistaa, että kirjoitettu koodi toimii odotetulla tavalla ja estää virheiden pääsemisen tuotantoon.

## Katso myös

- Clojure-testauksen perusteet: https://clojure.org/guides/getting_started#_testing
- Speclib-kirjaston dokumentaatio: https://github.com/slagyr/speclj
- Ohjelmistotestauksen merkitys: https://www.testimoi.fi/mita-testaus-on/