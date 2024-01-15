---
title:                "Testien kirjoittaminen."
html_title:           "Elm: Testien kirjoittaminen."
simple_title:         "Testien kirjoittaminen."
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä. Se varmistaa, että koodi toimii odotetusti ja auttaa havaitsemaan mahdollisia bugeja ennen kuin ne päätyvät tuotantoon.

## Kuinka

Kirjoittamalla testejä Elm:llä voit varmistaa, että koodisi toimii niin kuin on tarkoitus. Testit ovat kirjoitettu käyttäen Elm-testikirjastoa ja voivat automaattisesti suorittaa tarkistuksia koodin toimivuudesta. Alla on esimerkki yksinkertaisesta testistä:

```Elm
import Test exposing (..)
import Expect

-- Esitellään funktio, jonka haluamme testata
square : Int -> Int
square x =
  x * x

-- Kirjoitetaan testi
squareTest : Test
squareTest =
  test "neliöfunktion testi" <|
    \() ->
      Expect.equal (square 5) 25

-- Suoritetaan testi
main : Test
main =
  describe "Testit" [ squareTest ]
```

Tämä yksinkertainen testi varmistaa, että funktio nimeltä "square" palauttaa oikean arvon syötteelle 5. Voit lisätä haluamasi määrän testejä ja suorittaa ne kaikki yhdellä komennolla. Testien avulla voit myös luoda simulaatioita eri syötteistä ja varmistaa, että koodi käyttäytyy odotetulla tavalla.

## Syvempi sukellus

Testien kirjoittaminen Elm:llä ei ole vain hyödyllistä koodin toimivuuden varmistamiseksi, vaan se myös auttaa parantamaan koodin laatua ja ylläpidettävyyttä. Testien avulla voit eristää virheitä, jotka voivat ilmetä muutoksien yhteydessä ja helpottaa koodin muokkaamista tulevaisuudessa.

Testien kirjoittaminen Elm:llä seuraa yleensä kolmea vaihetta: syötteiden luominen, koodin suorittaminen ja odotusten asettaminen. Voit käyttää testikirjastoa lisätäksesi lisättyä monimutkaisuutta testien luomiseen tai voit luoda omia funktioita, jotka vastaavat yksinkertaisiin testitarpeisiisi.

## Katso myös

- [Elm-testikirjaston dokumentaatio](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm-oppaat ja esimerkit](https://elmprogramming.com/)