---
date: 2024-01-20 18:03:21.500746-07:00
description: "How to: - Kuinka tehd\xE4\xE4n: ."
lastmod: '2024-03-13T22:44:56.489152-06:00'
model: gpt-4-1106-preview
summary: .
title: Uuden projektin aloittaminen
weight: 1
---

## How to: - Kuinka tehdään:
```Elm
-- Uuden Elm-projektin pystyttäminen:
elm init

```
Tämä komento luo uuden Elm-projektin hakemistoon, jossa komentoa ajettiin, ja käynnistää `elm.json` tiedostonluonnin, joka määrittää projektin riippuvuudet.

```Elm
-- Uuden moduulin lisääminen:
module Main exposing (..)
import Html exposing (text)

main =
    text "Tervetuloa uuteen Elm-projektiisi!"

```
Lähdekoodiin lisätään uusi tiedosto, yleensä `Main.elm`, joka on projektin pääsyntymisen paikka.

## Deep Dive - Syväsukellus:
Elm perustuu funktionaalisen ohjelmoinnin periaatteisiin. Versiosta 0.19 lähtien, joka julkaistiin elokuussa 2018, Elm on keskittynyt tehokkuuteen ja 'dead code elimination' ominaisuuteen. 

Vaihtoehtoisesti Elm-projekteja voidaan luoda myös käyttäen erilaisia bootstrap-työkaluja kuten `create-elm-app` joka tarjoaa valmiita konfiguraatioita, mutta näiden käyttöön liittyy lisäkompleksisuutta.

Projektin aloituksessa on tärkeää määritellä `elm.json` tiedostossa, millaisia paketteja ja versioita projektisi tulee käyttämään. Elm:n pakettihallinta on tiukasti versioitu, jotta yhteensopivuusongelmia olisi mahdollisimman vähän.

## See Also - Katso Myös:
- [Elm:n viralliset dokumentaatiot](https://guide.elm-lang.org/)
- [create-elm-app](https://github.com/halfzebra/create-elm-app)
- [Elm paketti galleria](https://package.elm-lang.org/)
