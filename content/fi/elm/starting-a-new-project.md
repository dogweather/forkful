---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen tarkoittaa uuden sovelluksen tai ohjelman kehittämisen aloittamista tyhjästä. Ohjelmoijat tekevät tämän, koska se on tapa luoda räätälöityjä ratkaisuja, jotka vastaavat tiettyihin tarpeisiin.

## Kuinka:

Aloitetaan asentamalla Elm: 
```Bash
npm install -g elm
```
Sitten luodaan uusi projekti komennolla:
```Elm
elm init
```
Tämä luo `elm.json` tiedoston sekä `src` hakemiston. Sovelluksen pääfunktio sijoitetaan esimerkiksi `src/Main.elm` tiedostoon:
```Elm
module Main exposing (..)

import Html exposing (h1, text)


main =
    h1 [] [ text "Hello, Elm!" ]
```
Sovelluksen voi nyt ajaa selaimessa:
```Bash
elm reactor
```
Ja avata selaimessa osoitteella: [http://localhost:8000/src/Main.elm](http://localhost:8000/src/Main.elm)

## Syväsukellus

Elm lanseerattiin vuonna 2012 tuomaan parempaa luotettavuutta ja ylläpidettävyyttä web-sovelluskehitykseen. Elm tarjoaa vahvan tyypityksen ja ajonaikaisen virheiden poiston, jonka avulla ohjelmoijat voivat kehittää turvallisempia ja luotettavampia sovelluksia.

Vaihtoehtoisia tapoja uuden projektin aloittamiselle ovat esimerkiksi kehitettyjen projektimallien, kuten 'elm-spa-example' tai 'elm-architecture-tutorial' käyttö.

Hankkeen luomisessa, `elm init` komento luo konfiguraatiotiedoston `elm.json`, joka määrittää projektin riippuvuudet. Tämän lisäksi luodaan `src` hakemisto, johon Elm-lähdekooditiedostot, kuten `Main.elm`, sijoitetaan.

## Katso Myös

1. Elm-kielen kotisivu: https://elm-lang.org/
2. 'Building Web Apps with Elm' -kirja, jonka kirjoittanut James A. Garfield: https://pragprog.com/titles/jgelm/
3. Elm-ohjelmointiopas: https://elmprogramming.com/