---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:46.439533-07:00
description: "Testien kirjoittaminen Haskellilla koskee funktioidesi toivottujen tulosten\
  \ varmistamista automatisoitujen tarkistusten kautta. Ohjelmoijat tekev\xE4t sen\u2026"
lastmod: '2024-03-13T22:44:56.618595-06:00'
model: gpt-4-0125-preview
summary: Testien kirjoittaminen Haskellilla koskee funktioidesi toivottujen tulosten
  varmistamista automatisoitujen tarkistusten kautta.
title: Testien kirjoittaminen
weight: 36
---

## Mikä ja miksi?

Testien kirjoittaminen Haskellilla koskee funktioidesi toivottujen tulosten varmistamista automatisoitujen tarkistusten kautta. Ohjelmoijat tekevät sen löytääkseen virheet aikaisin, helpottaakseen refaktorointia ja dokumentoidakseen käyttäytymistä, mikä tekee koodikannasta ylläpidettävämmän ja skaalautuvamman.

## Kuinka:

Haskell tukee erilaisia testauskehyksiä, mutta kaksi suosittua ovat `Hspec` ja `QuickCheck`. Hspec antaa sinun määritellä ihmislukuisia erittelyjä koodillesi, kun taas QuickCheck antaa sinun automaattisesti generoida testejä kuvailemalla ominaisuuksia, joita koodisi tulisi tyydyttää.

### Käyttäen Hspeciä

Ensimmäisenä, lisää `hspec` rakennustyökalusi konfiguraatioon (esim. `stack.yaml` tai `cabal` tiedosto). Sen jälkeen, tuo `Test.Hspec` ja kirjoita testit eritelmiksi:

```haskell
-- tiedosto: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "lisää kaksi numeroa" $
    add 1 2 `shouldBe` 3

  it "palauttaa ensimmäisen numeron kun lisätään nolla" $
    add 5 0 `shouldBe` 5
```

Tämän jälkeen, aja testisi käyttäen rakennustyökaluasi, jolloin saat tuloksen, joka saattaa näyttää tältä:

```
MyLib.add
  - lisää kaksi numeroa
  - palauttaa ensimmäisen numeron kun lisätään nolla

Valmis 0.0001 sekunnissa
2 esimerkkiä, 0 epäonnistumisia
```

### Käyttäen QuickCheckiä

QuickCheckin kanssa ilmaiset ominaisuudet, jotka funktiosi tulisi tyydyttää. Lisää `QuickCheck` projektiisi konfiguraatioon, sitten tuo se:

```haskell
-- tiedosto: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Näiden testien ajaminen autogeneroi syötteitä tarkistamaan määritellyt ominaisuudet:

```
+++ OK, läpäisi 100 testiä.
+++ OK, läpäisi 100 testiä.
```

Sekä Hspec- että QuickCheck-esimerkeissä, testisarjat toimivat suoritettavina dokumentaatioina, jotka voivat automaattisesti varmentaa koodisi oikeellisuuden.
