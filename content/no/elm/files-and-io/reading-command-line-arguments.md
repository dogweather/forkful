---
date: 2024-01-20 17:55:58.007157-07:00
description: "Kommandolinjeargumenter lar brukere p\xE5virke et programs oppf\xF8\
  rsel ved oppstart. Vi bruker det for \xE5 tilpasse kj\xF8ringen av koden uten \xE5\
  \ endre selve\u2026"
lastmod: '2024-03-13T22:44:40.724247-06:00'
model: gpt-4-1106-preview
summary: "Kommandolinjeargumenter lar brukere p\xE5virke et programs oppf\xF8rsel\
  \ ved oppstart. Vi bruker det for \xE5 tilpasse kj\xF8ringen av koden uten \xE5\
  \ endre selve\u2026"
title: Lese kommandolinjeargumenter
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kommandolinjeargumenter lar brukere påvirke et programs oppførsel ved oppstart. Vi bruker det for å tilpasse kjøringen av koden uten å endre selve programmet.

## Slik gjør du:
Elm er primært en språk for webapplikasjoner og det kjører i nettleseren, ikke direkte i terminalen. Men du kan bruke JavaScript interoperability, kjent som ports, for å bygge en Elm-applikasjon som kommuniserer med Node.js for å håndtere kommandolinjeargumenter:

```Elm
port module Main exposing (..)

import Json.Decode as Decode
import Html

-- Definerer en port for å sende kommandolinjeargumentene til Elm fra JavaScript
port cmdArgs : (List String -> msg) -> Sub msg

-- Applikasjonsmodel
type alias Model =
    { args : List String }

-- Initialiserer modellen med en tom liste
init : Model
init =
    { args = [] }

-- Abonnerer på kommandolinjeargumenter
subscriptions : Model -> Sub msg
subscriptions model =
    cmdArgs NewArgs

-- Oppdaterer modell med argumentene
type Msg
    = NewArgs (List String)

update : Msg -> Model -> Model
update msg model =
    case msg of
        NewArgs args ->
            { model | args = args }

-- Viser argumentene
view : Model -> Html.Html Msg
view model =
    Html.text (String.join ", " model.args)

-- Enkel Elm-hovedfunksjon
main : Program () Model Msg
main =
    Html.program
        { init = (init, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
```
For JavaScript-koden som interopererer med Elm og Node.js, kan du bruke noe som dette:

```JavaScript
const { Elm } = require('./elm.js');

const app = Elm.Main.init();

// Sender kommandolinjeargumentene til Elm
const args = process.argv.slice(2);
app.ports.cmdArgs.send(args);
```

## Dypdykk
Å lese kommandolinjeargumenter i Elm direkte er ikke mulig fordi Elm kjører i nettleseren. I tidlige stadier handlet Elm hovedsakelig om ren funksjonell programmering for frontend-utvikling. Den tradisjonelle måten å håndtere kommandolinjeargumenter på skrivebordet ville vært terminalbaserte språk som Python eller Bash.

Men med Elm og Node.js kan du omgå denne begrensningen. Ports i Elm tillater toveis kommunikasjon med JavaScript, som igjen kan håndtere kommandolinjeargumentene i Node.js. Dette er et kraftfullt mønster som lar Elm-applikasjoner dra nytte av Node.js' server-side evner, inkludert lesing av kommandolinjeargumenter.

Et annet alternativ for å nå utenfor nettleseren med Elm har vært å bruke "elm-serverless", som lar utviklere deploye Elm kode som serverless funksjoner på infrastrukturer som AWS Lambda. Dette gir en spennende blanding av funksjonell renhet og sky-muligheter.

Implementasjonsdetaljer innebærer vanligvis å bruke `Node.js` sammen med Elm, hvor `Node.js` håndterer kommandolinjen og Elm kjører webapplogikken. Elm-koden og Node.js-snippeten må kobles sammen med en port-definisjon som vet hvordan den skal oversette informasjonen mellom de to miljøene.

## Se også
- Elm Ports dokumentasjon: https://guide.elm-lang.org/interop/ports.html
- Node.js prosessdokumentasjon: https://nodejs.org/api/process.html#process_process_argv
- "elm-serverless" for å lage serverless applikasjoner med Elm: https://www.elm-serverless.com/
