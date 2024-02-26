---
date: 2024-01-20 17:54:11.794529-07:00
description: "Att l\xE4sa en textfil inneb\xE4r att programmet h\xE4mtar data fr\xE5\
  n en fil som \xE4r lagrad p\xE5 disken. Programmerare g\xF6r detta f\xF6r att hantera\
  \ konfigurationer, l\xE4sa\u2026"
lastmod: '2024-02-25T18:49:36.140964-07:00'
model: gpt-4-1106-preview
summary: "Att l\xE4sa en textfil inneb\xE4r att programmet h\xE4mtar data fr\xE5n\
  \ en fil som \xE4r lagrad p\xE5 disken. Programmerare g\xF6r detta f\xF6r att hantera\
  \ konfigurationer, l\xE4sa\u2026"
title: "L\xE4sa en textfil"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil innebär att programmet hämtar data från en fil som är lagrad på disken. Programmerare gör detta för att hantera konfigurationer, läsa användardata eller importera externa data till sitt program.

## Hur gör man:
Elm har ingen inbyggd filsystemsfunktionalitet på klient-sidan av en webbapplikation p.g.a. webbsäkerhetsrestriktioner. Istället, använder du en `File`-modul som möjliggör att användaren kan välja filer som sedan kan läsas. Här är ett grundläggande exempel:

```Elm
module Main exposing (main)
import Browser
import File exposing (File)
import Html exposing (Html, button, div, input)
import Html.Events exposing (onClick)
import Task

type Msg
    = SelectFile (List File)
    | ReadFile (Result String String)

type alias Model =
    { fileContent : Maybe String }

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }

init : () -> (Model, Cmd Msg)
init _ =
    ( { fileContent = Nothing }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectFile files ->
            let
                file = List.head files
            in
            case file of
                Just selectedFile ->
                    ( model
                    , selectedFile
                        |> File.stringReader
                        |> Task.perform ReadFile
                    )

                Nothing ->
                    (model, Cmd.none)

        ReadFile result ->
            case result of
                Ok content ->
                    ( { model | fileContent = Just content }, Cmd.none )

                Err _ ->
                    ( { model | fileContent = Nothing }, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ input [ Html.Attributes.type_ "file", Html.Events.onInput (SelectFile << File.selectMultiple) ] []
        , div []
            (case model.fileContent of
                Just content ->
                    [ div [] [ Html.text content ] ]

                Nothing ->
                    []
            )
        ]
```

Här använder vi `File`-modulen för att läsa innehållet av en användarvald textfil och sedan visa innehållet på skärmen.

## Djupdykning:
I Elm, som i många funktionella språk, hanterar vi inte filsystemet direkt som i andra språk som Python eller JavaScript på serversidan. Historiskt sett är Elm skapat för att köras i webbläsaren där filsystemet är otillgängligt av säkerhetsskäl. 

Elm's filhantering på webben är begränsad till att användarna själva väljer vilken fil de vill att applikationen ska bearbeta. Detta sker genom en `input` av typen "file", som sedan kan läsas som en sträng eller omvandlas till andra format via filmodulen.

Som alternativ när det behövs server-side filhantering, kan man använda server-side Elm, vilket är mindre vanligt, eller integrera Elm-framsidan med ett backend i ett annat språk (t.ex. Node.js, Go, Ruby) som sköter filsystem-relaterade uppgifter.

## Se även:
- Elm File Module Documentation: [package.elm-lang.org/packages/elm/file/latest/](https://package.elm-lang.org/packages/elm/file/latest/)
- Discussions about file reading in Elm on Elm Discourse: [discourse.elm-lang.org/](https://discourse.elm-lang.org/)
