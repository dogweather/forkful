---
title:                "Haskell: Testien kirjoittaminen"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kun aloittaa ohjelmoinnin Haskellilla, testien kirjoittaminen voi tuntua turhalta tai aikaa vievältä vaiheelta. Mutta testien kirjoittaminen on todella tärkeää, sillä se auttaa varmistamaan koodin toimivuuden ja vähentää virheiden määrää. Kyse ei ole vain siitä, että koodin on pakko läpäistä testit, vaan testien avulla pystytään havaitsemaan ja korjaamaan ongelmia jo ennen kuin ne pääsevät tuotantoon.

## Miten kirjoitetaan testeja

Testien kirjoittaminen Haskellilla ei ole vaikeaa. Seuraavassa on muutama esimerkki, jotka näyttävät, kuinka voit käyttää `HSpec`- kirjastoa testien kirjoittamiseen.

```Haskell
-- Tuodaan tarvittava kirjasto
import Test.Hspec

-- Luodaan testi
main :: IO ()
main = hspec $ do
    describe "Laske-funktion testit" $ do
        it "Laskeaan 2 + 2" $
            laske 2 2 `shouldBe` 4

-- Määritellään testattava funktio
laske :: Int -> Int -> Int
laske x y = x + y
```

Koodin `shouldBe`-funktio vertaa koodin laskemyöhemmistä testin odotettuun tulokseen. Voit myös käyttää muita funktioita, kuten `shouldBeGreaterThan` tai `shouldSatisfy`, riippuen testin tarpeesta.

HSpec tarjoaa myös monia muita hyödyllisiä toimintoja, kuten `beforeAll` ja `afterAll`, joiden avulla voit suorittaa tiettyjä toimintoja ennen tai jälkeen testien suorittamisen. Voit tutustua näihin toimintoihin tarkemmin [HSpecin dokumentaatiosta](https://hspec.github.io/).

## Syventävä sukellus

Kun kirjoitat testejä, on tärkeää huomata, että testien on oltava itsenäisiä ja toisistaan riippumattomia. Yksi tapa varmistaa tämä on käyttää `beforeEach`-funktiota, joka suorittaa tietyt toiminnot ennen jokaista testiä. Näin voit varmistaa, että jokainen testi suorittaa samanläisillä oletusarvoilla ja testi ei vaikuta muiden testien tuloksiin.

Lisäksi testien tulisi kattaa mahdollisimman paljon erilaisia skenaarioita ja reittejä, jotta kaikki mahdolliset virhetilanteet pystytään havaitsemaan ja korjaamaan. Tämä vaatii huolellista suunnittelua ja testien jatkuvaa päivittämistä, jotta ne pysyvät ajantasalla koodin muutosten kanssa.

## Katso myös

- [HSpec-dokumentaatio](https://hspec.github.io/)
- [Haskell-testauskirjastoja](https://hackage.haskell.org/packages/search?terms=test)