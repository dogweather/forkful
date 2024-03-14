---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:09.011324-07:00
description: "Testien kirjoittaminen Clojurella, kuten muillakin ohjelmointikielill\xE4\
  , sis\xE4lt\xE4\xE4 omistautuneen koodin luomisen p\xE4\xE4koodikannan toimivuuden\
  \ varmistamiseksi.\u2026"
lastmod: '2024-03-13T22:44:56.187884-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen Clojurella, kuten muillakin ohjelmointikielill\xE4\
  , sis\xE4lt\xE4\xE4 omistautuneen koodin luomisen p\xE4\xE4koodikannan toimivuuden\
  \ varmistamiseksi.\u2026"
title: Testien kirjoittaminen
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
