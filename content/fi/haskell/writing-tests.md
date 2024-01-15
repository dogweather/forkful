---
title:                "Testien kirjoittaminen"
html_title:           "Haskell: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit kirjoittaa testeja? Yksinkertaisesti siksi, että se auttaa välttämään virheitä ja parantaa koodisi laatua. Testaaminen mahdollistaa myös koodin refaktoroinnin ja muokkaamisen helpommin ja turvallisemmin.

## Kuinka

Testien kirjoittaminen Haskellissa on helppoa ja intuitiivista. Tässä on muutama esimerkki ja tuloste näytteenomaisesta testikoodista:

```Haskell
-- Tuodaan testaamisen kirjasto käyttöön
import Test.HUnit

-- Esimerkki yksikkötestistä
testi1 = TestCase (assertEqual "Odotettu tulos" 5 $ 2 + 3)
-- Ajetaan testit
runTestTT testi1
```

Tämä yksikkötesti tarkistaa, että laskutoimitus 2 + 3 antaa tuloksen 5. Voit myös lisätä oman testifunktion, joka kutsuu muita funktioita ja testaa niiden paluuarvoja:

```Haskell
-- Yksinkertainen funktio
nelio x = x * x

-- Testi funktiolle
testi2 = TestCase (assertEqual "Nelion laskeminen" 9 $ nelio 3)
```

Nämä olivat vain yksi yksikkötestin esimerkki, mutta voit kirjoittaa useita erilaisia testejä eri funktioille ja niiden eri ominaisuuksille. Lisäksi voit käyttää muita testaamisen kirjastoja, kuten HSpecia, joka tarjoaa vieläkin monipuolisempia testausmahdollisuuksia.

## Syvenny

Haskellin testaaminen tarjoaa erilaisia lähestymistapoja ja näkökulmia, joihin voit syventyä. Voit esimerkiksi tutkia TDD:tä (Test Driven Development), jossa testit kirjoitetaan ennen itse koodia. Toinen mielenkiintoinen aihe on property-testaus, jossa testataan koodin ominaisuuksia satunnaisilla syötteillä.

Laadukkaan koodin kirjoittamiseen kuuluu myös testien kattavuuden tarkistaminen. Voit esimerkiksi käyttää työkaluja kuten HPC (Haskell Program Coverage), joka kertoo prosentuaalisesti, kuinka paljon koodistasi on testattu.

## Katso myös

- [Haskell testausdokumentaatio](https://www.haskell.org/documentation/#testing)
- [HSpec testaamisen kirjasto](https://hspec.github.io/)
- [TDD Haskellillä](https://dev.to/matbesancon/tdd-haskell-the-why-and-how-455g)