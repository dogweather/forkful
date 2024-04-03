---
date: 2024-01-20 17:59:44.554518-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel betyr \xE5 be en webserver om data\
  \ eller handlinger. Utviklere gj\xF8r dette for \xE5 hente, oppdatere eller slette\
  \ data p\xE5 nettet."
lastmod: '2024-03-13T22:44:40.705719-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel betyr \xE5 be en webserver om data eller\
  \ handlinger."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hva & Hvorfor?
Å sende en HTTP-forespørsel betyr å be en webserver om data eller handlinger. Utviklere gjør dette for å hente, oppdatere eller slette data på nettet.

## Hvordan:
Elm-koden under viser hvordan du sender en HTTP GET-forespørsel og håndterer svaret:

```Elm
module Main exposing (..)
import Browser
import Http
import Json.Decode as Decode

type alias Model =
    { responseText : String }

initialModel : Model
initialModel =
    { responseText = "" }

type Msg
    = GotText (Result Http.Error String)

update : Msg -> Model -> Model
update message model =
    case message of
        GotText (Ok text) ->
            { model | responseText = text }

        GotText (Err _) ->
            model

view : Model -> Html.Html Msg
view model =
    Html.text model.responseText

-- Sender en HTTP GET-forespørsel
sendGetRequest : Cmd Msg
sendGetRequest =
    Http.get
        { url = "https://api.example.com/data"
        , expect = Http.expectString GotText
        }

-- Starter applikasjonen og sender forespørselen
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, sendGetRequest )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }

```

Kjører du dette, vil programmet ditt be om data fra `https://api.example.com/data` og vise det som tekst.

## Dypdykk
HTTP-forespørsler i Elm handler om renhet og sikkerhet. Forespørsler er sideeffektfrie, og Elm tvinger håndtering av både suksess og feil. Historisk sett er Elm's HTTP-funksjoner inspirert av funksjonelle programmeringsidealer. Alternativer til Elm for HTTP-kommunikasjon inkluderer bruk av JavaScript gjennom "ports".

HTTP-forespørsler er håndterbare ved å sette opp `Http.expectWhatever` for å dekode svarene. Elm 0.19 introduserte `Http.expectString` som en enkel måte å få rå tekst. For kompleks data er det `Http.expectJson` med en `Decoder`. 

Tilstandene oppdateres rent. Alle svar går gjennom `update`-funksjonen, hvor du bestemmer hva som skal skje med den mottatte informasjonen. Elm er streng på typene som flyter gjennom systemet, og enhver feil ved utføring av en HTTP-forespørsel må håndteres.

## Se Også
- [Elm Guide on HTTP requests](https://guide.elm-lang.org/effects/http.html)
- [Elm Http package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [JSON decoding in Elm](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
