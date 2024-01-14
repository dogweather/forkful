---
title:                "Elm: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi?

Testaaminen on tärkeä osa koodin kirjoittamista, sillä se auttaa löytämään ja korjaamaan mahdollisia virheitä ja varmistaa, että koodi toimii oikein. Kirjoittamalla testejä varmistat myös, että muutokset ja lisäykset eivät aiheuta ongelmia jo kirjoitettuun koodiisi.

## Miten?

Testien kirjoittaminen Elm-kielellä on helppoa ja intuitiivista. Voit käyttää "elm-test" pakettia, joka tulee mukana Elm-asennuksen yhteydessä. Tässä esimerkissä luomme yksinkertaisen testin, joka tarkistaa, vastaako laskufunktiomme oikein:

```elm
-- Määritellään laskufunktio
laske : Int -> Int
laske x = x * 2

-- Tuodaan Elm-testipaketti
import Test exposing (..)

-- Luodaan testi
testi =
    describe "Laske-funktion testi"
        [ test "Laskennan tulos on oikein" <|
            \() ->
                Expect.equal (laske 5) 10
        ]

-- Suoritetaan testit
main : Program Never
main =
    beginnerProgram
        { model = ()
        , view = always ""
        , update = always
        , subscriptions = always Sub.none
        }

-- Testin tulos:
-- "1/1 passes"
```

## Syväsukellus

Testien kirjoittaminen Elm-kielellä noudattaa usein samaa kaavaa: määritellään funktio, tuodaan tarvittavat kirjastot ja luodaan testi, jossa verrataan saatuja tuloksia odotettuihin. Elm-test paketti tarjoaa myös muita käteviä funktioita testien kirjoittamiseen, kuten tarkistukset listoille ja merkkijonoille.

## Katso myös

- [Elm-test pakkauksen dokumentaatio](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm-test esimerkkejä](https://github.com/elm-explorations/test/tree/master/examples)
- [Elm-test kirjaston käyttö esimerkkikohteissa](https://blog.fsprojects.com/2020/07/03/elm-the-testing-learn-elm-with-tests/)