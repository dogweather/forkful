---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Elm: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on yksi tärkeimmistä ohjelmointitaidoista, joka voi auttaa sinua luomaan monimutkaisia ​​sovelluksia ja järjestelmiä. Se on myös hyödyllistä, kun haluat tallentaa tietoja pysyvästi ja jakaa niitä muiden kanssa.

## Miten

```Elm

module Main exposing (main)


import File
import Task


type Msg
    = GetFileContentReceived (Result File.Error String)


getFileContent : String -> Task.Task Never String
getFileContent fileName =
    File.readString fileName
        |> Task.map2 GetFileContentReceived


main : Program Never Model Msg
main =
    Task.attempt identity (getFileContent "tekstitiedosto.txt")


```

Tämä koodi ottaa käyttöön `File` -moduulin ja käyttää `Task.Task`-tyyppiä saadakseen tiedoston sisällön. `Elm`-tiedoston kirjoittamisen aloittamiseksi sinun on otettava käyttöön `File`-moduuli projektissasi ja käytettävä `Task`-tyyppiä.

## Syventävä sukellus

`File`-moduulissa on monia muita tärkeitä toimintoja, kuten tiedoston kirjoittaminen, poistaminen ja hakeminen. Voit myös käyttää `Json`-moduulia tallentamaan ja lukemaan tietoja JSON-muodossa.

`Elm`-tiedostojen kirjoittaminen on erittäin tärkeää, jos haluat luoda monimutkaisia ​​sovelluksia ja järjestelmiä. Se antaa sinulle mahdollisuuden tallentaa ja jakaa tietoja pysyvästi, mikä on olennaista monille sovelluksille. Jatkossa voit myös tutustua muihin `Elm`-kirjastoihin ja moduuleihin, joiden avulla voit tehdä enemmän tiedostoja ja tietoja käsitteleviä toimintoja.

## Katso myös

- [Virallinen `Elm`-verkkosivusto](http://elm-lang.org/)
- [Virallinen `Elm`-dokumentaatio](https://package.elm-lang.org/)
- [`Elm`-esimerkit GitHubista](https://github.com/elm/projects)