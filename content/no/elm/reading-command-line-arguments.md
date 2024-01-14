---
title:    "Elm: Å lese kommandolinje-argumenter"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Om du noen gang har programmert i Elm, har du sannsynligvis kommet bort i behovet for å lese inn argumenter fra kommandolinjen. Dette kan være nyttig for å tilpasse programmets oppførsel basert på brukerens input. I denne bloggposten vil jeg forklare hvorfor og hvordan man kan lese kommandolinjeargumenter i Elm.

## Hvordan

Å lese kommandolinjeargumenter i Elm er ikke komplisert og kan gjøres ved hjelp av den innebygde funksjonen `Platform.Cmdline.args`. Denne funksjonen returnerer en liste med argumentene som ble passert inn fra kommandolinjen.

For å bruke denne funksjonen, kan man først importere `Platform.Cmdline` modulen og deretter kalle `args` funksjonen slik:

```elm
import Platform.Cmdline

-- Leser inn kommandolinjeargumenter og lagrer dem i en variabel
args = Platform.Cmdline.args
```

La oss se på et eksempel for å forstå bedre. For å lese inn argumentene fra kommandolinjen og skrive dem ut i konsollen, kan vi bruke følgende kode:

```elm
import Platform.Cmdline
import Console

main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { args : List String
  }

init : () -> ( Model, Cmd Msg )
init _ =
  ( Model [], Cmd.none )

type Msg
  = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  -- Henter ut listen med argumenter og skriver dem ut i konsollen
  (Console.log "Argumenter:")
  (Platform.Cmdline.args
    |> List.map (Console.log << toString)
    |> List.mapHtml Console.text
  )

```

Når vi nå kjører programmet og legger til noen argumenter i kommandolinjen, vil vi se at argumentene blir skrevet ut i konsollen.

`$ elm reactor --port=3000`

I konsollen vil du da få følgende output:

```
Argumenter:
"reactor"
"--port=3000"
```

Med denne kunnskapen kan du nå lese inn og bruke kommandolinjeargumenter i dine egne Elm-prosjekter.

## Deep Dive

Det er verdt å merke seg at denne metoden for å lese kommandolinjeargumenter ikke støtter å lese inn flagg eller argumenter med flere ord. Hvis du trenger å lese inn slike argumenter, er det anbefalt å bruke en tredjeparts pakke som støtter dette, som for eksempel `elm-cmdargs`.

## Se også

- [Elm dokumentasjon for Platform.Cmdline](https://package.elm-lang.org/packages/elm/browser/latest/Browser#home)
- [Elm pakken elm-cmdargs](https://package.elm-lang.org/packages/brianbuchanan/elm-cmdargs/latest/)