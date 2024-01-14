---
title:                "Elm: Tiedostojen lukeminen"
simple_title:         "Tiedostojen lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisi tekstitiedostoa? Satunnaisena lukijana saatat ihmetellä, miksi kukaan haluaisi lukea pelkkiä tekstejä. Mutta me Elm-ohjelmoijat tiedämme, että tekstitiedostot voivat olla erittäin hyödyllisiä tietolähteitä ohjelmien sisällön tallentamiseen ja jakamiseen. Lue eteenpäin ja löydä, miten voit käyttää Elm:ää lukemaan tekstitiedostoja.

## Näin teet sen

Elm tarjoaa kätevän ```Text```-moduulin, jota voit käyttää tekstitiedoston lukemiseen. Alla on yksinkertainen esimerkki, jossa luemme tekstitiedoston ```data.txt``` ja tulostamme sen sisällön konsoliin:

```Elm
import Text exposing (..)

main =
  fileText "data.txt"
    |> andThen print

```

Tässä koodissa käytämme ```Text```-moduulin ```fileText```-funktiota, joka ottaa parametrinaan tekstitiedoston nimen ja palauttaa ```Task```-tyypin. Tämä ```Task``` suoritetaan ja sen lopputulos lähetetään ```andThen```-funktiolle, joka tulostaa tekstin haluamaamme paikkaan, tässä tapauksessa konsoliin.

Tällä tavalla voit lukea ja käsitellä tekstitiedostoja käyttäen Elmin ```Text```-moduulia.

## Syvempää sukellusta

Jos haluat keskittyä tarkemmin tekstitiedostojen lukemiseen Elm:llä, voit tutkia alla olevia linkkejä:

- [Elm Text -dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/Text)
- [Reaktiivinen ohjelmointi Elmin avulla](https://guide.elm-lang.org/effects/)
- [Elm-yhteisön foorumi](https://discourse.elm-lang.org/)

## Katso myös

- [Elm-kielen virallinen sivusto](https://elm-lang.org/)
- [Elm-oppaan aloitussivu](https://guide.elm-lang.org/)
- [Elm-kielen oppituntivalikoima](https://elm-lang.org/learn)