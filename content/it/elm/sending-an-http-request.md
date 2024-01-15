---
title:                "Invio di una richiesta http"
html_title:           "Elm: Invio di una richiesta http"
simple_title:         "Invio di una richiesta http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore Elm, ti chiederai sicuramente perché qualcuno dovrebbe inviare una richiesta HTTP tramite questo linguaggio. La risposta è semplice: l'invio di richieste HTTP è fondamentale per comunicare con server e risorse esterne, permettendoti di creare applicazioni web dinamiche e interattive.

## Come Fare
Per inviare una richiesta HTTP in Elm, è necessario utilizzare la libreria `elm/http`. Una volta importata, puoi utilizzare la funzione `Http.send` per specificare il metodo di richiesta, l'URL e gli eventuali dati da inviare. Ad esempio:

```Elm
import Http
import Json.Encode as Json

sendRequest : Http.Request
sendRequest =
    Http.send
        { method = "GET"
        , url = "https://api.example.com/users"
        , body = Http.emptyBody
        , expect = Http.expectJson (Json.list userDecoder)
        }
```

In questo esempio, stiamo inviando una richiesta `GET` all'URL specificato e ci aspettiamo una risposta in formato JSON. Inoltre, possiamo utilizzare la funzione `Http.post` per inviare dati a un server tramite una richiesta `POST`.

Una volta che la nostra richiesta è stata creata, dobbiamo ancora inviarla effettivamente utilizzando la funzione `Http.send` all'interno della funzione `update` del nostro modello.

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendRequest ->
            ( model, Http.send sendRequest )
```

Quando la risposta viene ricevuta, la funzione `Http.expectJson` ci permette di utilizzare un decoder personalizzato per estrarre i dati che ci interessano dal payload.

## Approfondimento
In Elm, inviare una richiesta HTTP è un processo semplice e sicuro grazie al sistema di tipizzazione forte del linguaggio. Inoltre, la libreria `elm/http` gestisce automaticamente gli errori di connessione e ci fornisce un feedback strutturato sui possibili problemi durante l'invio o la ricezione di una richiesta.

## Vedi Anche
- Documentazione ufficiale di Elm su invio di richieste HTTP: https://guide.elm-lang.org/effects/http.html
- Tutorial su invio di richieste HTTP in Elm: https://medium.com/@rtfeldman/your-first-web-app-in-elm-8e247744d5f3
- Esempi di codice per inviare richieste HTTP in Elm: https://github.com/rtfeldman/elm-spa-example