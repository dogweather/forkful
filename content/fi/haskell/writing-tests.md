---
title:    "Haskell: Testien kirjoittaminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa testejä?

Testien kirjoittaminen voi vaikuttaa aikaa vievältä ja tarpeettomalta, mutta ne ovat todella tärkeitä varmistettaessa koodin laatu ja toiminnallisuus. Testit voivat auttaa havaitsemaan virheitä ja korjaamaan ne ennen kuin ne pääsevät tuotantoon, mikä säästää aikaa ja vaivaa pitkällä aikavälillä.

# Kuinka kirjoittaa testejä Haskellilla?

Testien kirjoittaminen Haskellilla on helppoa ja intuitiivista käyttäen HSpec-kirjastoa. Seuraavassa on näytekoodi, jossa määritellään yksinkertainen testi ja sen odotettu tulos:

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ describe "LaskeSumma" $ do
    it "Laskee kahden numeron summan oikein" $ do
        laskeSumma 2 3 `shouldBe` 5
```

Koodin tulos näyttää seuraavanlaiselta:

```
LaskeSumma
  Laskee kahden numeron summan oikein

Finished in 0.0006 seconds
1 example, 0 failures
```

Tässä koodissa käytetään `describe`- ja `it`-funktioita, jotka auttavat selkeämmän ja helpommin luettavan koodin luomisessa. Koodin `shouldBe`-funktio vertaa lasketun summan odotettuun tulokseen ja ilmoittaa testeistä, jotka eivät ole menneet läpi.

# Syvempi sukellus

Testien kirjoittaminen voi olla monimutkaisempaa kuin edellä esitetyssä esimerkissä. Haskellilla on monia erilaisia testauskirjastoja, jotka tarjoavat erilaisia ominaisuuksia, kuten HUnit, QuickCheck ja SmallCheck. Ne kaikki toimivat eri tavoin ja voivat tarjota erilaisia etuja testien kirjoittamisessa.

Toinen tärkeä osa testien kirjoittamista on kattavuus. Kattavuustestit auttavat varmistamaan, että kaikki koodin osat on testattu ja kattavat mahdollisimman paljon. Tätä voi tehdä käyttämällä työkaluja, kuten HPC (Haskell Program Coverage) ja HSpecin kattavuusraportteja.

On myös tärkeää huomata, että testien kirjoittaminen voi auttaa kehittämään parempaa ja modulaarisempaa koodia. Kun otetaan huomioon testit jo suunnitteluvaiheessa, se auttaa hahmottamaan paremmin koodin rakennetta ja vähentää virheiden mahdollisuutta.

# Katso myös

- [Haskell Test's homepage](https://www.haskell.org/)
- [HSpec documentation](https://hspec.github.io/)
- [HPC documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-hpc.html)