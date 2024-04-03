---
date: 2024-01-20 17:55:53.139027-07:00
description: "Komennon rivin argumenttien lukeminen tarkoittaa sy\xF6tteiden vastaanottamista\
  \ k\xE4ynnistyksen yhteydess\xE4. Ohjelmoijat k\xE4ytt\xE4v\xE4t t\xE4t\xE4 toimintoa,\
  \ koska se\u2026"
lastmod: '2024-03-13T22:44:56.503802-06:00'
model: gpt-4-1106-preview
summary: "Komennon rivin argumenttien lukeminen tarkoittaa sy\xF6tteiden vastaanottamista\
  \ k\xE4ynnistyksen yhteydess\xE4."
title: Komennoriviparametrien lukeminen
weight: 23
---

## Näin toimit:
Elm ei suoraan tue komentorivin argumenttien käsittelyä, koska se on suunniteltu pääasiassa web-kehitykseen. Tämän sijaan, voit käyttää JavaScriptin `process.argv` ominaisuutta Elm-ohjelmasta käsin käyttämällä portteja (ports). Tässä on esimerkki kuinka voit saada argumentit Elm-ohjelmaasi:

```Elm
port module Main exposing (..)

import Browser
import Json.Decode as Decode

-- Määritä portti komentorivin argumenttien vastaanottamiseen
port cmdlineArgs : (List String -> msg) -> Sub msg

-- Käynnistä ohjelma
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- Aloita tilan hallinta
init : () -> (Model, Cmd Msg)
init _ =
    -- Alusta malli ilman argumentteja
    (Model [], Cmd.none)

type Msg
    = ReceiveArgs (List String)

-- Päivitä tila
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveArgs args ->
            -- Tallenna vastaanotetut argumentit malliin
            (Model args, Cmd.none)

-- Malli, joka sisältää komentorivin argumentit
type alias Model =
    { args : List String }

-- Tilaus komentorivin argumenttien kuunteluun
subscriptions : Model -> Sub Msg
subscriptions model =
    cmdlineArgs ReceiveArgs

-- (Tyhjä) näkymä
view : Model -> Html msg
view model =
    -- Käytä tässä osaa nähdäksesi argumentit web-sivulla
    Html.text ""
```

Lisää JavaScript-koodisi index.html tiedostoon:

```JavaScript
const app = Elm.Main.init({
  node: document.getElementById('elm')
});

// Lähetä komentorivin argumentit Elmiin portin kautta
app.ports.cmdlineArgs.send(process.argv);
```

## Syväsukellus
Elm ei suoraan tarjoa tapaa lukea komentorivin syötteitä, koska kielessä keskitytään selkeisiin web-sovelluksiin. Tämä on yksi suurista erityispiirteistä Elmissä verrattuna muihin ohjelmointikieliin, jotka on suunniteltu yleiskäyttöön, kuten Python tai Node.js. Jos tarvitset komentorivin argumentteja Elmissä, yllä kuvattu JavaScript-porttiratkaisu on tyypillinen tapa yhdistää Elm ja Node.js ympäristöt.

## Katso Myös
- Elm-portit: https://guide.elm-lang.org/interop/ports.html
- Elm ja Node.js: https://elm-lang.org/news/interop-with-node-js-and-electron
- JSON-dekoodaus Elmissä: https://package.elm-lang.org/packages/elm/json/latest/
