---
title:    "Clojure: Testausten kirjoittaminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen voi vaikuttaa aikaa vievältä ja turhalta, mutta se on itse asiassa tärkeä osa laadukkaan ja toimivan koodin kirjoittamista. Testaaminen auttaa meitä havaitsemaan virheitä ja varmistamaan, että koodimme toimii odotetulla tavalla. Se myös helpottaa uusien ominaisuuksien lisäämistä ja koodin ylläpitämistä tulevaisuudessa.

## Kuinka tehdä

Testien kirjoittaminen Clojurella on helppoa ja nopeaa. Seuraavassa on esimerkki yksinkertaisesta testifunktiosta, joka tarkistaa, onko annettu luku parillinen vai ei:

```Clojure
(defn parillinen? [luku]
  (if (even? luku)
    true
    false))
```

Testin tulisi palauttaa `true` jos annettu luku on parillinen ja `false` jos se on pariton. Voimme testata tätä seuraavasti:

```Clojure
(deftest testi-parillisuudesta
  (testing "testaa parittoman luvun"
    (is (not (parillinen? 3))))
  (testing "testaa parillisen luvun"
    (is (parillinen? 4))))
```

Nämä testit odottavat, että `parillinen?`-funktio palauttaa odotetun tuloksen. Jos kaikki menee suunnitellusti, testien tulisi läpäistä ja näemme seuraavan tulosteen:

```Clojure
lein test
lein test user-test 

lein test testi-parillisuudesta7 failure in 1 test (user_test.clj:7)
```


FAILURE in (testi-parillisuudesta) (user_test.clj:7)
expected: (not (parillinen? 3))
  actual: false

Tässä näemme, että ensimmäinen testi epäonnistui, koska se odotti `true`-arvoa, mutta saikin `false`. Korjatkaamme `parillinen?`-funktio toimimaan oikein ja ajakaamme testit uudelleen:

```Clojure
(defn parillinen? [luku]
  (if (even? luku)
    true
    false))

(deftest testi-parillisuudesta
  (testing "testaa parittoman luvun"
    (is (not (parillinen? 3))))
  (testing "testaa parillisen luvun"
    (is (parillinen? 4))))

```

Tulosteen pitäisi nyt näyttää tältä:

```
lein test
lein test user-test 

Ran 1 tests containing 2 assertions.
0 failures, 0 errors.
```

## Syväsukellus

Testit eivät vain auta meitä havaitsemaan virheitä, vaan myös varmistavat, että koodimme on helposti muokattavissa ja ylläpidettävissä. Kirjoittamalla testejä voimme helposti lisätä uusia ominaisuuksia ja varmistaa, että vanhat ominaisuudet eivät mene rikki.

Jotkut hyödyllisiä työkaluja testien kirjoittamiseen Clojurella ovat Clojure testikirjasto sekä [Midje](https://github.com/marick/Midje)-kirjasto. Midje tarjoaa mm. testien automaattisen suorittamisen jokaisen tallennuksen yhteydessä, mikä helpottaa testien kirjoittamista ja ylläpitoa.

## Katso myös

- [Clojure testikirjaston dokumentaatio](https://clojure.github.io/clojure/clojure.test-api.html)
- [Midje-kirjaston GitHub-sivu](https://github.com/marick/Midje)