---
title:                "Testien kirjoittaminen"
aliases:
- fi/clojure/writing-tests.md
date:                  2024-02-03T19:30:09.011324-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Testien kirjoittaminen Clojurella, kuten muillakin ohjelmointikielillä, sisältää omistautuneen koodin luomisen pääkoodikannan toimivuuden varmistamiseksi. Se auttaa tarkkuuden varmistamisessa, refaktoroinnin helpottamisessa ja koodin vakauden parantamisessa.

## Kuinka:
Clojure, hyödyntäen JVM:ää, tukee erilaisia testauskehyksiä. Kuitenkin yleisesti käytetty sisäänrakennettu kirjasto on `clojure.test`. Tässä on yksinkertainen esimerkki:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Lisäysfunktio"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Tämän testin suorittamisen jälkeen näkisit tulosteen, joka muistuttaisi:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

Niille, jotka etsivät ominaisuuksiltaan rikkaampia vaihtoehtoja, voi käyttää kolmannen osapuolen kirjastoja, kuten `Midje` tai `test.check`. Tässä on, miten voisit käyttää Midjeä vastaavaan testiin:

Lisää ensin Midje projektisi clj-riippuvuuksiin:
```clojure
[midje "1.9.9"]
```

Sitten testisi Midjen kanssa saattaisi näyttää tältä:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Testataan lisäystä"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Testin suorittamisen jälkeen Midjen kautta komennolla `lein midje`, tuloste näyttäisi jotakin vastaavaa:

```
Kaikki tarkistukset (2) onnistuivat.
```
