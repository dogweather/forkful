---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaaminen tarkoittaa ohjelmasi toiminnan varmistamista automatisoiduilla kokeilla. Ohjelmoijat testaavat koodinsa välttääkseen bugeja ja parantaakseen koodin laatua.

## How to:
Haskellissa testit kirjoitetaan usein käyttäen HUnit- tai QuickCheck-kirjastoja. Tässä yksinkertainen esimerkki HUnitilla:

```Haskell
import Test.HUnit

-- Funktio, joka kertoo kaksi numeroa
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Testi funktiolle
testMultiply :: Test
testMultiply = TestCase (assertEqual "Should multiply two numbers" 20 (multiply 4 5))

-- Testien suoritustoiminto
main :: IO ()
main = runTestTT testMultiply >>= print
```

Suorita testi komennolla `runhaskell test.hs`. Tulostus kertoo testin tuloksen.

## Deep Dive:
Haskellin testaustyökalut ovat kehittyneet vuosien varrella. Alkuperäisen HUnitin rinnalle ovat nousseet QuickCheck ja hedgehog, jotka mahdollistavat satunnaistetut testit. Näiden avulla voimme luoda kattavampia testejä erilaisille syötteille. Testien ajamisessa yleensä käytetään Stack- tai Cabal-työkaluja, jotka mahdollistavat testien automatisoinnin osana rakennusprosessia.

## See Also:
- HUnit-kirjaston kotisivu: https://hackage.haskell.org/package/HUnit
- QuickCheck-kirjasto: https://hackage.haskell.org/package/QuickCheck
- Hedgehog-kirjasto: https://hackage.haskell.org/package/hedgehog
- Haskell Stack-dokumentaatio: https://docs.haskellstack.org/en/stable/README/
- Cabal-käyttöopas: https://www.haskell.org/cabal/users-guide/
