---
title:    "Clojure: Testien kirjoittaminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi: Miksi testien kirjoittaminen kannattaa?

Testien kirjoittaminen on tärkeä osa ohjelmoinnin prosessia, sillä se auttaa varmistamaan, että koodi toimii oikein ja vähentää potentiaalisten bugien määrää. Ilman testejä koodin muutokset ja uudet ominaisuudet voivat helposti rikkoa vanhaa toimivaa koodia.

## Miten: Esimerkkejä testien kirjoittamisesta Clojurella

Yksi tapa kirjoittaa testejä Clojurella on käyttää testikirjastoa nimeltä `clojure.test`. Se mahdollistaa yksikkötestien kirjoittamisen ja suorittamisen helposti.

```Clojure
;; Tässä testissä tarkistetaan, että funktio laskee oikein kertoma faktaorialle 5
(deftest test-faktaorio
  (testing "Faktaorialaskun tulos on oikein"
    (is (= (faktaorio 5) 120))))
```

Testin suorittamisen jälkeen näemme tuloksen:

```
Testing app.test-faktaorio

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

Testaamme myös toista funktiota, joka palauttaa tietyn indeksin arvon listasta:

```Clojure
(deftest test-indeksi
  (testing "Palauttaa oikean indeksin listasta"
    (let [lista [1 2 3 4 5 6]]
      (is (= (get-indeksi lista 3) 4)))))
```

Tulos:

```
Testing app.test-indeksi

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Syväsukellus: Lisätietoa testien kirjoittamisesta

Yksikkötestien lisäksi on myös mahdollista kirjoittaa integraatiotestejä Clojurella. Integraatiotesteillä varmistetaan, että eri komponentit toimivat yhdessä odotetulla tavalla. Ne voivat myös auttaa löytämään mahdollisia ongelmia esimerkiksi tietokantakyselyissä.

Testien kirjoittamisen lisäksi on tärkeää myös suunnitella testausstrategia, jotta testit kattavat mahdollisimman laajan osan koodista. Testien tulisi myös olla itsenäisiä, jotta yhden testin kaatuminen ei vaikuta muihin. Lisäksi on hyvä seurata testien kattavuutta ja varmistaa, että uuden koodin lisääminen ei vähennä testien kattavuutta.

## Katso myös
- Tietoa Clojuren testikirjastosta `clojure.test`: https://clojure.github.io/clojure/clojure.test-api.html
- Hyviä käytäntöjä testien kirjoittamiseen: https://clojure.org/guides/testing_best_practices
- Lisätietoa integraatiotesteistä: https://enterprisecraftsmanship.com/2015/01/07/integration-testing-your-clojure-applications/