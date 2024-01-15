---
title:                "Lese kommandolinjeargumenter"
html_title:           "Elm: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være lurt å lære å lese kommandolinjeargumenter når man lærer å programmere i Elm. Dette er fordi mange programmer bruker kommandolinjeargumenter for å motta input fra brukeren og dermed kunne skreddersy programmet til deres behov.

## Hvordan

Det finnes flere måter å lese kommandolinjeargumenter på i Elm, men en av de enkleste og mest intuitive måtene er å bruke "Cmd.map" funksjonen. Dette lar oss definere en funksjon som tar imot argumentene som input og deretter behandler dem slik vi ønsker.

```Elm
import Platform.Cmd exposing (map)
import Task exposing (attempt)

type Msg
    = GotArguments (List String)

main : Program Nil Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.map GotArguments Platform.Cmd.arguments )

type alias Model = List String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotArguments arguments ->
            ( arguments, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

```

I kodeeksempelet over bruker vi funksjonen "Platform.Cmd.arguments" for å hente ut en liste med kommandolinjeargumenter. Deretter mapper vi denne listen til vår "GotArguments" melding og behandler den i vår "update" funksjon.

For å se en fullstendig løsning kan du sjekke ut dette kodeeksempelet: [Kommandolinjeargumenter i Elm](https://ellie-app.com/5wJ65xbYWP3a1)

## Deep Dive

Ved å bruke "Cmd.map" funksjonen kan vi ikke bare hente ut en liste med argumenter, men også spesifisere hvilken type data vi ønsker å hente. For eksempel kan vi bruke "Platform.Cmd.argumentsAs" for å hente ut en liste med tall i stedet for strenger.

Det finnes også andre måter å behandle kommandolinjeargumenter på, som for eksempel å bruke en "command-line-args" pakke fra pakkesystemet "Elm Packages".

## Se også

- [Kommandolinjeargumenter i Elm](https://ellie-app.com/5wJ65xbYWP3a1)
- [Elm pakkesystem](https://guide.elm-lang.org/install/elm.html) 
- [Pakken "command-line-args" for behandling av kommandolinjeargumenter](https://package.elm-lang.org/packages/the-sett/command-line-args/latest)