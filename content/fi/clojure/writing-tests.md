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

## Miksi

Miksi joku haluaisi kirjoittaa testejä ohjelmointikoodiinsa? Tämä voi tuntua turhalta ja aikaa vievältä, mutta todellisuudessa testit ovat erittäin hyödyllisiä ja säästävät aikaa ja vaivaa pitkällä aikavälillä. Testien kirjoittamisen avulla voit varmistaa, että koodisi toimii odotetusti ja välttää mahdollisia bugeja ja virheitä.

## Kuinka

### Käyttötapaukset testaamiselle

Ensinnäkin, on tärkeää ymmärtää, milloin ja miksi sinun pitäisi testata koodiasi. Joskus testaaminen voi olla tarpeetonta, varsinkin jos kyseessä on yksinkertainen ohjelma tai pieni koodipätkä. Kuitenkin, jos projektisi on monimutkainen ja/tai jaettu useaan eri osaan, testien kirjoittaminen voi olla hyödyllistä ja jopa välttämätöntä.

Toiseksi, sinun pitäisi miettiä, mitä haluat testata. Voit esimerkiksi haluta varmistaa, että tietty funktio toimii odotetulla tavalla tai että tiettyjä reunaehtoja käsitellään oikein. On myös hyvä miettiä, millaisia tuloksia testiltäsi odotat ja miten testituloksia tulisi tulkita.

### Toteutus

Testien kirjoittaminen Clojurella on suhteellisen helppoa ja suoraviivaista. Voit käyttää kahta eri tapaa testien kirjoittamiseen: joko käyttämällä Clojuren sisäänrakennettua [```clojure.test```](https://clojure.github.io/clojure/clojure.test-api.html) kirjastoa tai käyttämällä suositumpaa [```expectations```](https://github.com/clojure-expectations/expectations) kirjastoa.

Alla on esimerkki testin kirjoittamisesta Clojuren sisäänrakennetulla kirjastolla:

```clojure
(ns example.core-test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (is (= 10 (addition 4 6))))
```

Testi hyödyntää Clojuren ```is``` -funktiota, joka tarkastaa, että kaksi arvoa ovat samat. Tässä tapauksessa testi varmistaa, että ```addition``` -funktio tuottaa oikean tuloksen, kun sille annetaan parametreiksi 4 ja 6.

Käyttämällä expectations-kirjastoa, testi voisi näyttää tältä:

```clojure
(expect (= 10 (addition 4 6)))
```

Expectations-teknologia on hieman helpompi ja intuitiivisempi verrattuna sisäänrakennettuun ```clojure.test``` -kirjastoon. Valinta on lopulta oma mieltymyksesi mukaan.

## Syventyvä tarkastelu

Testien kirjoittaminen voi tuntua turhalta työltä, varsinkin jos projektisi on vielä alkuvaiheessa. Kuitenkin, ajan myötä ja projektin kasvaessa, testit ovat erittäin tärkeitä, kun haluat varmistaa, että uudet muutokset eivät riko jo olemassa olevaa toiminnallisuutta.

On myös hyvä huomata, että testien kirjoittaminen auttaa myös koodin ymmärtämisessä ja ylläpitämisessä. Kun sinulla on selkeästi määritellyt testit, voit käyttää niitä myös dokumentointina koodiasi kohtaan. Lisäksi, testejä voi käyttää myös debuggaamisessa, kun haluat selvitt