---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Luodaan Väliaikaisia Tiedostoja Elm-Ohjelmointikielessä

Elm-ohjelmointikieli on hienostunut JavaScript-korvaaja, jonka avulla voit kirjoittaa interaktiivisia käyttöliittymiä turvallisesti ja tehokkaasti. Tässä artikkelissa käydään läpi, kuinka voit luoda väliaikaisia tiedostoja Elm-ohjelmointikielessä.

## Mikä & Miksi?

Luodaan väliaikainen tiedosto tarkoittaa tiedoston luomista tietokoneen välimuistiin. Käytämme väliaikaisia tiedostoja, kun haluamme tallentaa joitakin tietoja vain istunnon ajaksi; nämä tiedostot voidaan turvallisesti poistaa, kun istunto päättyy.

## Kuinka Toimia:

Elm-ympäristössä ei ole sisäänrakennettua funktiota väliaikaisten tiedostojen luomiseen, mutta voimme toteuttaa tämän käyttämällä Web API:n Blob-objektia. Käytämme myös FileSaver.js kirjasto, joka antaa meille mahdollisuuden tallentaa tiedostoja käyttäjän laitteelle.

```Elm
port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

port download : Value -> Cmd msg

type alias Model =
    {}

type Msg
    = Download

main =
    Browser.sandbox { init = {}, update = update, view = view }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Download ->
            ( model, download <| Encode.string "Tiedoston sisältö" )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Download ] [ text "Lataa tiedosto" ] ]
```

Huomaa, että tarvitset julkisen portaalin (`port download`) lähettämään tiedot JS:lle, ja sen jälkeen FileSaver.js hoitaa tiedoston tallennuksen.

## Syvempi Sukellus

Väliaikaisten tiedostojen luonti on peräisin ajasta, jolloin etäpalvelimet tai tehokkaat työasemat tekivät laskentatehtäviä ja käyttäjät yksinkertaisesti kirjoittivat tai lukevat tiedostoja etäyhteyden avulla. 

Sen sijaan, että Elm tarjoaisi väliaikaisten tiedostojen luontia, Elm keskittyy ennemmin puhtaan funktionaaliseen ohjelmointiin ja turvallisen ja tehokkaan käyttöliittymän luomiseen.

Väliaikaisten tiedostojen luomiselle on vaihtoehtoisia lähestymistapoja, kuten käyttämällä julkaistuja rajapintoja tiedostojen käsittelyyn tai käyttämällä JavaScript-kirjastoja rajapintoina.

## Katso myös

- [Elm-kielen opas, Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
- [FileSaver.js dokumentaatio](https://github.com/eligrey/FileSaver.js/)
- [Elm Web API Moduuli](https://package.elm-lang.org/packages/elm/browser/latest/Browser)