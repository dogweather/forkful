---
title:                "Elm: Komentoriviparametrien lukeminen"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan uutta blogikirjoitustamme, jossa käsitellään komentoriviparametrien lukemista Elm-ohjelmointikielellä. Jos olet kiinnostunut oppimaan uutta ja parantamaan ohjelmointitaitojasi Elm:ssä, tämä artikkeli on juuri sinulle.

## Miten

Komentoriviparametrien lukeminen on tärkeä taito jokaiselle Elm-kehittäjälle. Tässä esimerkissä näytämme, miten voit lukea komentoriviltä annetut parametrit ja tulostaa ne konsoliin.

```Elm
module Main exposing (..)

import Platform
import Task exposing (Task)
import Task exposing (succeed)

main : Program Never Model Msg
main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { arguments : List String
  }

type Msg
  = NoOp

init : ( Model, Cmd Msg )
init =
  ( Model arguments = getArguments () ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

view : Model -> Html Msg
view model =
  div []
    [ text ("Komentoriviltä saadut parametrit: " ++ String.join " " model.arguments)
     ]

getArguments : () -> List String
getArguments () =
  Platform.worker
    (Task.perform succeed (\args -> args) Task.commandLineArguments)
```

Tulostus:

```
Komentoriviltä saadut parametrit: elm-blogi lukija
```

## Syväsukellus

Nyt kun olet oppinut perusteet komentoriviparametrien lukemisesta Elm:ssä, voit tutustua syvemmin tähän aiheeseen. Voit esimerkiksi etsiä lisää tietoa Task-moduulista ja sen tarjoamista toimintoista tai tutkia tarkemmin, miten komentoriviparametrit toimivat ja miten niitä voi käyttää ohjelman suorituksen aikana.

## Katso myös

- [Task-moduuli Elm:ssä](https://package.elm-lang.org/packages/elm/core/latest/Task)
- [Cmd-modulei Elm:ssä](https://package.elm-lang.org/packages/elm/core/latest/Cmd)
- [Elm-dokumentaatio](https://guide.elm-lang.org/)
- [Komentoriviparametrit käskyjen ohjelmointikielessä](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)

Kiitos lukemisesta ja toivottavasti tästä oli sinulle hyötyä Elm-ohjelmoinnissa!