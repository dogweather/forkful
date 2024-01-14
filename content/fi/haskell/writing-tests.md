---
title:    "Haskell: Testien kirjoittaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi kirjoittaa testeja?

Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä monella tapaa. Ensinnäkin se auttaa varmistamaan, että koodi toimii odotetulla tavalla ja korjaa mahdolliset virheet ennen kuin ne pääsevät tuotantoon. Lisäksi testien avulla voi arvioida koodin suorituskykyä ja löytää mahdollisia optimointimahdollisuuksia. Lopuksi testien avulla voidaan varmistaa, että koodi pysyy toimivana myös tulevien muutosten yhteydessä.

## Miten kirjoittaa testeja?

Testien kirjoittaminen Haskellissa on melko helppoa, ja seuraavassa on muutamia esimerkkejä siitä, miten testejä voi kirjoittaa omassa koodissasi.

```Haskell
import Test.HUnit

-- Yksinkertainen yhteenlaskutesti
testi1 = TestCase (assertEqual "1 + 2 ei ole 3" 3 (1+2))

-- Testitapauksien lista
testit = TestList [testi1]

-- Testaa kaikki testitapaukset
main = runTestTT testit
```

Output:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
```

Edellä olevassa esimerkissä käytämme Test.HUnit-moduulia, joka tarjoaa meille tarvittavat työkalut testien kirjoittamiseen. `TestCase`-funktio ottaa testin nimen ja testin tuloksen, ja `assertEqual` verraa odotettua tulosta ja todellista tulosta. Voit myös luoda oman `Test`-tyypin ja käyttää sitä `TestList`-funktiolla luomaan testitapauksista listan. Lopuksi `runTestTT` käynnistää kaikki testit ja tulostaa niiden tulokset.

## Syvällinen sukellus

Testien kirjoittamisessa on monia muita hyviä käytäntöjä, kuten testikattavuuden mittaaminen ja nimien antaminen testitapauksille kuvaavasti. Lisäksi kannattaa tutustua myös QuickCheck-kirjastoon, joka tarjoaa keinoja satunnaisesti generoida testitapauksia.

## Katso myös

- [HUnit-dokumentaatio](https://hackage.haskell.org/package/HUnit)
- [QuickCheck-dokumentaatio](https://hackage.haskell.org/package/QuickCheck)
- [Good Practices for Writing Unit Tests in Haskell](https://medium.com/free-code-camp/effective-unit-testing-in-haskell-with-quickcheck-hunit-and-hspec-6f3a0af5795d) (englanniksi)