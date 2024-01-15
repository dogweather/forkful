---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Elm: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Kommentorivin argumenttien lukeminen on tärkeä taito, joka helpottaa ohjelmien suorittamista ja hallintaa. Tämän taidon avulla voit helposti välittää parametreja ohjelmalle sen suorituksen aikana.

## Kuinka

```Elm
import Platform exposing (Program)
import Task exposing (Task)
import Parser exposing (run, string, succeed) 

main : Program Never Model 
main = 
    Platform.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Alustaa ohjelman lukemaan komentorivin argumenttien rivin
init: (Model, Cmd Msg)
init = (Model "", Task.succeed Cmd")

-- Näyttää yksinkertaisen tekstikentän
view : Model -> Html Msg
view model =
    text model

-- Päivittää tilan komentorivilla annettuun arvoon
type Msg = Cmd String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cmd arg ->
            (Model arg, Cmd.none)

-- Tilaukset komentorivin argumenttien muutoksille
subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none

-- Toteuttaa komentorivin argumenttien lukemisen
port currentUrl : String
port currentUrl =
    toTask Platform.Task

-- Käynnistää ohjelman komentorivilla annetun argumentin avulla
port launch : Task x
port launch = 
    Succeed ()

-- Mahdollistaa ohjelman suorittamisen komentorivin argumentteja apuna käyttäen
port toTask : (String -> Task x) -> Sub x
port toTask task =
  on "args" (map task currentUrl)
```

```bash
elm make Main.elm
elm reactor
```

#### Syöte: `elm reactor`  
#### Output: Palvelin käynnistyy oletuksena portissa 8000.

#### Syöte: `elm make Main.elm --output=app.js`  
#### Output: Luo `app.js` -tiedoston.

#### Syöte: `elm make Main.elm --optimize`  
#### Output: Optimoi tiedostokoon.

#### Syöte: `elm repl`  
#### Output: Käynnistää Elm-tulkin, jolla voit kokeilla koodia reaaliajassa.

## Syvemmälle

Kommentorivin argumenttien lukeminen perustuu tietokoneen käyttöjärjestelmän ominaisuuksiin. Komentorivin argumenttien lukemisessa on myös huomioitava mahdolliset virhetilanteet, kuten virheellisten argumenttien antaminen ja odotettujen argumenttien puuttuminen. Jokaisella käyttöjärjestelmällä voi myös olla omat ominaisuutensa ja tapansa käsitellä komentorivin argumentteja, joten kannattaa tutustua niihin ennen koodin kirjoittamista.

## Katso myös
- [Elm-ohjelmointikielen virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Komentoriviparametrien käsittely eri käyttöjärjestelmissä](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)