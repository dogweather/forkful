---
date: 2024-01-20 17:59:35.392926-07:00
description: 'How to: Segui questo esempio per mandare una richiesta GET in Elm.'
lastmod: '2024-03-13T22:44:43.347041-06:00'
model: gpt-4-1106-preview
summary: Segui questo esempio per mandare una richiesta GET in Elm.
title: Inviare una richiesta http
weight: 44
---

## How to:
Segui questo esempio per mandare una richiesta GET in Elm:
```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

getUser : Cmd Msg
getUser =
    Http.send ReceiveUser
        (Http.get
            { url = "https://api.example.com/user/1"
            , decoder = userDecoder
            }
        )

type Msg
    = ReceiveUser (Result Http.Error User)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveUser (Ok user) ->
            ( { model | user = Just user }, Cmd.none )

        ReceiveUser (Err _) ->
            ( { model | error = Just "Failed to fetch user." }, Cmd.none )
```

Esegui `getUser` e aspettati un output simile quando ricevi una risposta:
```Elm
{ id = 1, name = "Mario Rossi" }
```

## Deep Dive
HTTP in Elm è gestito dallo `Http` package. La storia degli HTTP requests risale al 1991, quando il web nasceva. Oggi, `Http.get` e `Http.post` sono ampiamente usati in Elm, insieme a decoders per interpretare le risposte. Alternativamente, usare il tipo `Http.request` dà più controllo. I programmatori Elm devono gestire gli effetti collaterali, quindi le richieste HTTP sono comandi eseguiti nel `update` function che restituiscono messaggi definiti dall'utente.

## See Also
Per approfondire:
- Documentazione ufficiale `Http`: <https://package.elm-lang.org/packages/elm/http/latest>
- Guida agli `Json.Decoders`: <https://guide.elm-lang.org/interop/json.html>
- Esempi Elm `Http`: <https://elm-lang.org/examples/http>
