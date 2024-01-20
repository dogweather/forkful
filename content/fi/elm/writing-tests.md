---
title:                "Ohjelmointitestien kirjoittaminen"
html_title:           "Elm: Ohjelmointitestien kirjoittaminen"
simple_title:         "Ohjelmointitestien kirjoittaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Testien kirjoittaminen on tärkeä osa ohjelmointia, sillä se auttaa varmistamaan koodin toimivuuden ja vähentää virheiden mahdollisuutta. Ohjelmoijat kirjoittavat testejä varmistaakseen, että heidän koodinsa toimii oikein ja pysyy toimivana myös tulevaisuudessa.

## Miten:
Elm-kielellä testien kirjoittaminen on helppoa ja tehokasta. Alla on muutamia esimerkkejä, miten voit kirjoittaa testejä Elm:llä ja mitä tulee tulos näyttämään.

```
Elm-testi

import Test exposing (..)
import Expect exposing (expect)

testit : Test
testit =
    describe "Testaa kaksi lukua"
        [ test "Lukujen summa on oikein" <|
            \() ->
                expect (1 + 2) toBe 3
        , test "Lukujen erotus on oikein" <|
            \() ->
                expect (5 - 2) toBe 3
        , test "Lukujen kertolasku on oikein" <|
            \() ->
                expect (4 * 3) toBe 12
        ]

```

Tulos:
```
Testata kaksi lukua
    ✓ Lukujen summa on oikein
    ✓ Lukujen erotus on oikein
    ✓ Lukujen kertolasku on oikein

Passed: 3, Failed: 0
```

## Syvemmälle:
Testaaminen on tärkeä osa ohjelmointia jo pitkään ollut käytäntö. Se auttaa ohjelmoijia löytämään ja korjaamaan virheitä nopeasti ja välttämään mahdollisia ongelmia tulevaisuudessa. Elm-kielellä on monia muitakin testaamistyökaluja, kuten elm-test-rs, joka tarjoaa helpon tavan suorittaa testejä projektissasi. Lisäksi voi myös olla hyödyllistä lukea lisää testaamisesta yleisesti ja löytää parhaita käytäntöjä testaamiseen.

## Katso myös:
- [Testaaminen yleisesti](https://en.wikipedia.org/wiki/Software_testing)