---
title:                "Haskell: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Haskell on yksi suosituimmista funktionaalisista ohjelmointikielistä, jota käytetään laajasti monissa sovelluksissa. Yksi tärkeä osa laadukkaan Haskell-koodin kirjoittamista on testaaminen. Testien kirjoittaminen varmistaa, että koodimme toimii odotetulla tavalla ja auttaa vähentämään virheitä. Seuraava opas auttaa sinua ymmärtämään, miksi testaaminen on tärkeää ja miten voit aloittaa sen käytön Haskell:ssa.

## Miten aloittaa

Haskellissa testien kirjoittaminen on suhteellisen helppoa käyttämällä HSpec-kirjastoa. Ensiksi sinun tulee asentaa HSpec komentoriviltä käyttämällä `cabal install hspec` tai `stack install hspec` riippuen mitä paketinhallintajärjestelmää käytät. Tämän jälkeen voit aloittaa testien kirjoittamisen luomalla uuden tiedoston ```Testit.hs``` ja kirjoittamalla seuraavan esimerkin sisään:

```
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "nelioJuuri" $ do
    it "laskee neliöjuuren oikein" $ do
      nelioJuuri 4 `shouldBe` 2
```

Tässä yksinkertaisessa koodissa määritämme `nelioJuuri` funktion ja testaamme sen toimivuutta käyttämällä `shouldBe` funktiota. Nyt suorittamalla tämän tiedoston komentoriviltä käyttäen `runhaskell Testit.hs`, näet seuraavan tulosteen:

```
nelioJuuri
  laskee neliöjuuren oikein

Finished in 0.0012 seconds
1 example, 0 failures
```

Näet näinollen, että testi on onnistunut ja `nelioJuuri` funktio laskee neliöjuuren oikein.

## Syvemmälle testaukseen

Nyt kun olet aloittanut testien kirjoittamisen Haskell:ssa, voit syventää tietämystäsi ja käyttää erilaisia testaustyökaluja. HSpec tarjoaa mahdollisuuden lisätä erilaisia ehtoja testaukseen, kuten testien ryhmittelyä käyttämällä `describe`, `context` ja `it` lohkoja. Voit myös testata monimutkaisempia funktioita tai jopa testata sisäisiä tiloja käyttämällä `IO` toimintoja. On tärkeää huomata, että testien kirjoittaminen ei tarkoita, että koodisi ei koskaan epäonnistuisi, mutta se auttaa löytämään virheitä ja varmistamaan sovelluksesi toimivuuden.

## Katso myös

- [HSpec-dokumentaatio](https://hspec.github.io/)
- [Haskell-testaus eli päätoiminnan arvon tutkiminen](https://haskell-doc.gitbooks.io/haskell-testaus/content/)
- [Funktionaalinen testaus Haskell-kielen bibliografian näkökulmasta](http://birchsport.drcut.com/~mathematica/book/title/funtestbookend.html)