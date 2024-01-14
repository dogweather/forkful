---
title:                "Elm: Lesing av kommandolinjeargumenter"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kunne lese kommandolinje-argumenter er en viktig ferdighet for enhver Elm-utvikler. Det lar deg enkelt få tilgang til og behandle data som brukes til å tilpasse eller kontrollere utførelsen av ditt Elm-program.

# Hvordan

For å lese kommandolinje-argumenter i Elm, må du importere Cmd-modulen og bruke funksjonen `Cmd.worker`, som lar deg legge til en kommando som skal kjøres når programmet starter.

```Elm
import Cmd exposing (worker)

main =
    webContent
        |> Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

init () =
    ( Model "", Cmd.none)

update msg model =
    case msg of
        SetArgument argument ->
            ( Model argument, Cmd.none )

type Msg
    = SetArgument String

view model =
    Html.text ("Kommandolinje-argument som ble gitt til programmet var: " ++ model)

subscriptions model =
    worker SetArgument Cmd.Runtime.arguments
```

Her har vi en enkel `Model` som bare lagrer argumentet som blir gitt til programmet. Dette argumentet blir satt til `Model` ved å bruke `SetArgument`-meldingen i `update`-funksjonen. `subscriptions`-funksjonen vår bruker `worker` for å registrere kommandolinje-argumentet og sende det som `SetArgument`-melding.

Når programmet blir kjørt, vil `Model`-teksten vise kommandolinje-argumentet som ble gitt.

# Deep Dive

Videre kan vi utvide funksjonaliteten vår ved å tillate å lese flere argumenter. Dette kan gjøres ved å bruke `Cmd.map` for å mappe flere argumenter til en liste. Vi kan også sjekke om et spesifikt argument ble gitt ved å bruke `Debug.todo`-funksjonen for å utløse en feil dersom argumentet ikke finnes.

```Elm
subscriptions model =
    worker SetArguments (Cmd.map List.filterRuntime.arguments)

update msg model =
    case msg of
        SetArguments arguments ->
            ( Model arguments, Cmd.none )
        UnknownArgument ->
            ( model, Cmd.none )

init () =
    ( Model [], Cmd.map List Runtime.arguments)

case List.member "myargument" arguments of
            True ->
                ( , Cmd.none )
            False ->
                ( model, Cmd.perform UnknownArgument Debug.todo "Missing required command line argument: myargument" )
```

Med denne tilnærmingen vil vår `Model` holde en liste over alle kommandolinje-argumentene som ble gitt til programmet.

# Se Også

Hvis du vil lære mer om å jobbe med kommandolinje-argumenter i Elm, kan du se følgende ressurser:

- Dokumentasjon for Cmd-modulen: https://package.elm-lang.org/packages/elm/core/latest/Cmd
- Elm-guide: https://guide.elm-lang.org/
- Elm-programmeringsspråkets offisielle nettside: https://elm-lang.org/