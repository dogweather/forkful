---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:32.533374-07:00
description: "Testien kirjoittaminen Elm-kielell\xE4 k\xE4sitt\xE4\xE4 testitapausten\
  \ laatimisen Elm-koodisi oikeellisuuden varmistamiseksi, jotta se toimii odotetusti.\
  \ Ohjelmoijat\u2026"
lastmod: '2024-03-13T22:44:56.492123-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen Elm-kielell\xE4 k\xE4sitt\xE4\xE4 testitapausten\
  \ laatimisen Elm-koodisi oikeellisuuden varmistamiseksi, jotta se toimii odotetusti."
title: Testien kirjoittaminen
weight: 36
---

## Mikä ja miksi?

Testien kirjoittaminen Elm-kielellä käsittää testitapausten laatimisen Elm-koodisi oikeellisuuden varmistamiseksi, jotta se toimii odotetusti. Ohjelmoijat tekevät näin löytääkseen virheitä aikaisin, helpottaakseen ylläpitoa ja parantaakseen sovellustensa laatua ja luotettavuutta.

## Kuinka:

Elm käyttää `elm-explorations/test`-pakettia yksikkö- ja sumutestien kirjoittamiseen. Aloita lisäämällä paketti projektiisi:

```elm
elm install elm-explorations/test
```

Luo testitiedosto, sanotaan `tests/ExampleTest.elm`, ja tuo testausmoduulit. Tässä on yksinkertainen testi, joka varmistaa funktion `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Yksinkertainen summatoiminto"
        [ test "Lisäämällä 2 ja 3 saadaan 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Testiesi ajamiseen tarvitset `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Tämä kääntää testisi ja tulostaa tulokset terminaaliisi. Ylläolevasta esimerkistä tulostus pitäisi olla jotakin seuraavanlaista:

```
TESTIAJO LÄPI

Kesto: 42 ms
Läpäiset:   1
Hylätyt:   0
```

Monimutkaisemman esimerkin osalta, sanotaan että haluat sumutestata `add` funktion varmistaaksesi, että se käsittelee oikein laajan valikoiman kokonaislukusyötteitä. Sinun tulisi muokata `ExampleTest.elm` tiedostoasi seuraavasti:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testataan add sumutuksella"
        [ fuzz int "Sumutestaus add satunnaisilla kokonaisluvuilla" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Aja `elm-test` uudelleen nähdäksesi sumutestien toiminnan. Tuloste vaihtelee satunnaisten syötteiden mukaan, mutta onnistuneet testit osoittavat, ettei hylkäyksiä ole:

```
TESTIAJO LÄPI

Kesto: 183 ms
Läpäiset:   100
Hylätyt:   0
```

Nämä esimerkit näyttävät, kuinka kirjoittaa ja ajaa yksinkertaisia yksikkö- ja sumutestejä Elm-kielellä käyttäen `elm-explorations/test`-pakettia. Testaus on elintärkeä osa kehitysprosessia, auttaen varmistamaan että Elm-sovelluksesi ovat luotettavia ja ylläpitävät korkeaa laatua.
