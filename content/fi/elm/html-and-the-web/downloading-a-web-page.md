---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/elm/downloading-a-web-page.md
date:                  2024-01-20T17:44:00.683070-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Webbisivun lataaminen tarkoittaa sivun sisällön hakemista netistä. Ohjelmoijat lataavat sivuja tiedon keräämiseen, automatisointiin, tai interaktiivisten web-sovellusten rakentamiseen.

## How to:
Lataa sivu Elm:ssä HTTP-paketin kanssa. Vastaanota ja käsittele tieto.

```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

type Msg
    = GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText (Ok str) ->
            ({ model | content = str }, Cmd.none)

        GotText (Err _) ->
            (model, Cmd.none)

model : Model
model =
    { content = "" }

init : () -> (Model, Cmd Msg)
init _ =
    (model, Http.get { url = "http://example.com", expect = Http.expectString GotText })

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    Html.text model.content

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
```
Kun koodi käynnistetään, se lataa `example.com` sivuston sisällön ja näyttää sen.

## Deep Dive
Elm tuli julkiseksi 2012. Sen funktiopohjainen lähestymistapa sopii hyvin web-sovellusten rakentamiseen turvallisesti ja ennustettavasti. Elm:n HTTP-paketti tarjoaa funktiot webbin datan lataamiseen.

Vaihtoehtoja Elm:n HTTP-paketille löytyy, kuten JavaScript-rajapinnat (`fetch`, `XMLHttpRequest`), mutta Elm:n oma syntaksi minimoi sovelluksen mahdolliset virhetilat.

Elm käyttää `Cmd` tyyppiä sivuvaikutusten, kuten HTTP-pyyntöjen, hallintaan. Tämä erottaa puhtaan laskennan sivuvaikutusten aiheuttamasta logiikasta.

## See Also
- Elm Language Guide for HTTP: https://guide.elm-lang.org/effects/http.html
- Elm Package for HTTP: https://package.elm-lang.org/packages/elm/http/latest/ 
- Elm discourse for community support: https://discourse.elm-lang.org/

Tutustu myös näihin resursseihin syventääksesi osaamistasi Elm:n HTTP-toiminnallisuuksissa.
