---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:01:47.287386-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Å sende en HTTP-forespørsel med grunnleggende autentisering betyr at man legger til brukernavn og passord i forespørselen for tilgangskontroll. Programmerere gjør dette for å sikre at kun autoriserte brukere får tilgang til bestemt data.

## How to:
```Elm
import Http
import Base64

type alias Model =
    { response : String }

basicAuth : String -> String -> Http.Header
basicAuth username password =
    let
        credentials =
            Base64.encode (username ++ ":" ++ password)
    in
    Http.header "Authorization" ("Basic " ++ credentials)

sendRequestWithBasicAuth : Cmd Msg
sendRequestWithBasicAuth =
    Http.get
        { url = "https://eksempel.com/data"
        , headers = [ basicAuth "brukernavn" "passord" ]
        }
        |> Http.expectWhatever ResponseHandler
```

Forventet output:
```Elm
type Msg
    = ResponseHandler (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResponseHandler (Ok data) ->
            ( { model | response = data }, Cmd.none )

        ResponseHandler (Err _) ->
            ( { model | response = "Feil under henting av data." }, Cmd.none )
```

## Deep Dive
Bakgrunnen for grunnleggende autentisering innen HTTP går tilbake til tidlig internett, hvor enkel tilgangssikring var nødvendig. Mens modernere metoder som OAuth 2.0 eller JWT (JSON Web Tokens) nå ofte er foretrukket for deres styrke og fleksibilitet, kan grunnleggende autentisering fortsatt være nyttig for enkel server-til-server kommunikasjon eller for små prosjekter som ikke krever avansert sikkerhet.

Når det gjelder implementering i Elm, bruker vi `Http`-pakken for å bygge og sende forespørsler, mens `Base64`-pakken hjelper oss med å kode brukernavn og passord. Husk at grunnleggende autentisering sender legitimasjonen i klartekst, bare kodet med Base64, så det bør alltid brukes over en sikker HTTPS-forbindelse.

## See Also
- Elm `Http`-pakken: https://package.elm-lang.org/packages/elm/http/latest/
- Elm `Base64`-pakken: https://package.elm-lang.org/packages/truqu/elm-base64/latest/
- Autentiseringsstandarder: https://developer.mozilla.org/docs/Web/HTTP/Authentication
