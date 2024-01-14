---
title:    "Elm: Testien kirjoittaminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa testejä? Testien kirjoittaminen voi vaikuttaa ylimääräiseltä työltä, mutta se voi myös tuoda merkittäviä etuja. Testien avulla voit varmistaa, että koodisi toimii oikein ja vähentää bugeja sekä parantaa koodin laatua ja ylläpidettävyyttä. Testien avulla voit myös helposti havaita ongelmia ja virheitä, mikä säästää aikaa ja vaivaa kehitysprosessin aikana.

## Miten tehdä

Testien kirjoittaminen Elm-kielellä on helppoa ja intuitiivista. Alla on esimerkki yksinkertaisesta testistä, joka tarkistaa, onko annettu numero parillinen vai ei.

``` Elm
module Main exposing (..)

import Test exposing (..)
import Test.Assertions exposing (expect)

isEven : Int -> Bool
isEven x =
    x % 2 == 0

tests : Test
tests =
    describe "Even Test"
        [ test "4 is even" <|
          \() -> expect (isEven 4) (equalTo True)
        , test "3 is not even" <|
          \() -> expect (isEven 3) (equalTo False)
        ]

main : Program Test
main =
    testRunner tests
```

Käyttämällä `Test`-moduulia voit määrittää testitapaukset `describe`-funktiolla ja suorittaa testit `testRunner`-funktiolla. Voit myös käyttää `expect`-funktiota määrittämään odotetun tuloksen ja käyttämällä `equalTo`-funktiota määrittämään, millainen tulos tulisi olla.

Ajamalla tämän esimerkkikoodin, saat seuraavan tulosteen:

```
> TEST RUN PASSED

  Even Test
    ✓ 4 is even
    ✓ 3 is not even

  2 tests passed in 0ms
  List []

Tests passed
```

## Syvempi sukellus

Testien kirjoittamisen lisäksi on tärkeää myös varmistaa, että testisi ovat tehokkaita ja kattavat kaikki tärkeät osat koodistasi. Tärkeitä asioita, joita tulisi ottaa huomioon testien kirjoittamisessa, ovat mm. testien nimeäminen selkeästi, erilaisten syötteiden ja reunaehtojen testaaminen sekä testikattavuus eli se, kuinka monta prosenttia koodistasi on testattu.

Lisäksi on tärkeää huomata, että testien kirjoittaminen ei tarkoita sitä, että koodisi olisi täysin bugiton. Sen sijaan testien tarkoituksena on auttaa sinua havaitsemaan ja korjaamaan mahdollisia virheitä mahdollisimman varhaisessa vaiheessa. Siksi on tärkeää myös huolehtia siitä, että koodisi on mahdollisimman selkeää ja ymmärrettävää.

## Katso myös

- [Elm Test Library](https://package.elm-lang.org/packages/elm-explorations/test/latest/) - Testikirjasto, joka helpottaa testien kirjoittamista
- [Testing Elm Applications](https://github.com/mpizenberg/mcget/Testing Elm Applications) - Opas testien kirjoittamiseen Elm-sovelluksissa