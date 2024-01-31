---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Testikoodaus tarkoittaa koodin automaattista testaamista virheiden varalta. Koodarin on helppo tarkistaa, toimiiko kaikki oikein eri tilanteissa, ja se säästää aikaa jatkokehityksessä.

## How to:
Elm-test on työkalu koodin testaamiseen Elm-ohjelmissa. Asenna elm-test ja luo testit näin:

```Elm
import Expect
import Test exposing (..)
import YourModule exposing (..)

suite : Test
suite =
  describe "YourModule"
    [ test "2 + 2 equals 4" <|
        \_ -> 2 + 2 |> Expect.equal 4
    , test "reverseString 'moi' equals 'iom'" <|
        \_ -> reverseString "moi" |> Expect.equal "iom"
    ]

-- To run this test use the elm-test command from your terminal.
```

Tulokset näyttävät tältä, kun ajat testit komentoriviltä:
```
TEST RUN PASSED

Duration: 42 ms
Passed:   2
Failed:   0
```

## Deep Dive
Testaus Elmissä pohjautuu puhtaasti funktionaaliseen näkemykseen ohjelmoinnista. Elm-test perustuu fuzz-testaukseen, mikä tarkoittaa satunnaisten, mutta relevanteilla tavoin syötettyjen, datojen testaamista. Tämä eroaa monista imperatiivisista tai OOP-kieleistä, joissa yksikkötestaus on yleisempää. Elm-testin käyttöä tukee vahva tyypitysjärjestelmä, mikä vähentää tarvetta tietyntyyppisille testeille.

## See Also
- Elm-test paketti: [https://package.elm-lang.org/packages/elm-explorations/test/latest](https://package.elm-lang.org/packages/elm-explorations/test/latest)
- Elm-testin käyttöönotto: [https://medium.com/@_rchaves_/writing-tests-in-elm-2c714ffe5c52](https://medium.com/@_rchaves_/writing-tests-in-elm-2c714ffe5c52)
