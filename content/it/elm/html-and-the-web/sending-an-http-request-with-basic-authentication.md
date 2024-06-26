---
date: 2024-01-20 18:01:23.725371-07:00
description: 'How to - Come fare: Output dopo una request riuscita.'
lastmod: '2024-04-05T21:53:44.120703-06:00'
model: gpt-4-1106-preview
summary: Output dopo una request riuscita.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## How to - Come fare:
```Elm
import Http
import Base64

type alias Model =
    { response : String }

type Msg
    = GotData (Result Http.Error String)

basicAuth : String -> String -> List Http.Header
basicAuth username password =
    let
        encodedCredentials =
            Base64.encode (username ++ ":" ++ password)
    in
    [ Http.header "Authorization" ("Basic " ++ encodedCredentials) ]

sendRequest : Cmd Msg
sendRequest =
    Http.get
        { url = "https://your-api/endpoint"
        , headers = basicAuth "yourUsername" "yourPassword"
        , expect = Http.expectString GotData
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotData (Ok data) ->
            ( { model | response = data }, Cmd.none )

        GotData (Err _) ->
            ( { model | response = "Failed to fetch data." }, Cmd.none )

-- Include the rest of your Elm module implementation here.
-- Remember to initialize the model and start the application.
```

Output dopo una request riuscita:
```
{ response = "Dati Riservati Qui." }
```

Output dopo un errore:
```
{ response = "Failed to fetch data." }
```

## Deep Dive - Immersione Profonda
L'autenticazione di base HTTP è uno dei modi più semplici per controllare l'accesso alle risorse web. Inventata ben presto nella storia del web, è tuttora largamente usata nonostante sia meno sicura di altri metodi, poiché le credenziali sono codificate in Base64 ma non criptate. E' importante trasmettere queste richieste esclusivamente su HTTPS.

Alternative all'autenticazione di base includono OAuth, che fornisce token di accesso invece di username e password, e l'autenticazione tramite API key.

Nell'esempio di Elm sopra, `basicAuth` costruisce un'intestazione che incapsula le credenziali codificate in Base64. `Http.get` effettua la richiesta con l'intestazione e aspetta una stringa come risposta. La gestione dell’aggiornamento del modello in base al risultato della richiesta avviene nella funzione `update`.

## See Also - Vedi Anche
- Documentazione Elm su HTTP: [Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/)
- Tutorial di Base64 in Elm: [Elm Base64](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
- Introduzione all'autenticazione OAuth: [OAuth](https://oauth.net/2/)
