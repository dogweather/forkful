---
title:                "Elm: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché
Sending an HTTP request è un'importante abilità per gli sviluppatori Elm. Può essere utilizzato per ottenere dati da un server o per interagire con altre risorse web. 

## Come fare
Per inviare una richiesta HTTP in Elm, è necessario utilizzare il pacchetto `elm/http`. È possibile creare una richiesta utilizzando la funzione `Http.request` e specificare l'URL desiderato, il metodo HTTP e gli eventuali parametri. Ecco un esempio di codice:

```elm
import Http
import Json.Decode exposing (..)

type Msg = GotData (Result Http.Error String)

sendRequest : Cmd Msg
sendRequest =
  Http.request
    { method = "GET"
    , url = "https://jsonplaceholder.typicode.com/todos/1"
    , expect = expectJson (\_ -> GotData)
    }
```

Nell'esempio sopra, stiamo inviando una richiesta GET all'URL `https://jsonplaceholder.typicode.com/todos/1` e ci aspettiamo una risposta in formato JSON. Quando la risposta arriva, la funzione `expectJson` viene eseguita e il risultato viene passato alla nostra funzione di gestione dei messaggi `GotData`.

## Deep Dive
Oltre all'URL e al metodo HTTP, è possibile specificare anche Intestazioni, Parametri e Contenuti per la richiesta. È anche possibile gestire gli errori utilizzando la funzione `expectString` o `expectWhatever` per elaborare i diversi tipi di risposta previsti. Per saperne di più su come gestire le richieste HTTP in modo efficace, si consiglia di consultare la documentazione ufficiale su [elm/http](https://package.elm-lang.org/packages/elm/http/latest/).

## Vedi anche
- [Documentazione ufficiale di Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/)
- [Tutorial su come inviare richieste HTTP in Elm](https://pusher.com/tutorials/consume-restful-api-elm)
- [Esempio di applicazione Elm che utilizza richieste HTTP](https://github.com/rtfeldman/elm-spa-example)