---
title:    "Elm: Lesing av kommandolinje-argumenter"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har programmert i Elm, har du kanskje lagt merke til at noen programmer har mulighet for å lese inn kommandolinjeargumenter. Dette kan være nyttig for å gi programmene våre forskjellige funksjonaliteter basert på hva som blir skrevet inn i kommandolinjen. I denne bloggposten skal vi se på hvordan man kan lese kommandolinjeargumenter i Elm, og hvorfor dette kan være nyttig.

## Hvordan gjøre det

For å lese inn kommandolinjeargumenter i Elm, bruker vi den innebygde funksjonen `platform.program`. Denne funksjonen tar inn en `Program` som parameter, og sørger for at programmet vårt får tilgang til kommandolinjeinformasjonen. La oss se på et enkelt eksempel:

```Elm
module Main exposing (main)

import Platform exposing (program)
import String exposing (split)

main : Program Cmd msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
        
type alias Model =
    { args : List String
    }
    
init : () -> (Model, Cmd msg)
init _ =
    let
        args =
            platform.program.metadata.args
    in
    ({ args = split " " args }, Cmd.none)
    
update : msg -> Model -> (Model, Cmd msg)
update msg model =
    (model, Cmd.none)
    
subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
```

Her oppretter vi et enkelt program med en `Model` som inneholder en liste av alle kommandolinjeargumentene som blir sendt inn til programmet. Vi bruker `platform.program.metadata.args` til å returnere argumentene som en enkelt streng, og deretter bruker vi `String.split`-funksjonen til å dele opp strengen og skape en liste av argumentene.

For å kjøre programmet vårt og lese inn kommandolinjeargumenter, skriver vi inn følgende kommando i terminalen:

```bash
elm app.elm --arg1 arg2 arg3
```

Her vil `app.elm` være navnet på filen vår, mens `arg1`, `arg2` og `arg3` er argumentene som blir sendt inn til programmet. Vi kan deretter bruke disse argumentene i vårt program for å gi ulike funksjonaliteter eller tilpasse oppførselen basert på dem.

## Dypdykk

`platform.program.metadata`-funksjonen tilbyr også andre nyttige informasjon, som for eksempel hvilken nettleser som blir brukt, og hvilken versjon av Elms kjører på. Du kan også bruke `platform.program.context`-funksjonen til å få tilgang til informasjon som nettleserens språkinnstillinger og skjermoppløsning.

Det er også verdt å nevne at denne funksjonen kun fungerer når programmet blir kjørt i en nettleser, og ikke når det blir kjørt lokalt i en Elm REPL.

## Se også

- [Elm Platform Dokumentasjon](https://guide.elm-lang.org/install/README.html)
- [Elm Kommandolinjeargumenter Eksempel](https://www.kuuttila.net/2020/07/25/ellie-samples-web.html)