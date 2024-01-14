---
title:                "Elm: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Molti sviluppatori scelgono di utilizzare Elm come loro linguaggio di programmazione preferito per diversi motivi: è funzionale, tipizzato staticamente e offre una grande esperienza agli sviluppatori. Uno dei benefici di Elm è la possibilità di effettuare richieste HTTP con autenticazione di base per comunicare con server remoti in modo sicuro e affidabile.

## Come Fare

Effettuare una richiesta HTTP con autenticazione di base in Elm è un processo semplice, ma prima bisogna comprendere alcuni concetti chiave. Innanzitutto, bisogna importare il modulo `Http` e `Json.Decode` per utilizzare le funzioni necessarie per effettuare una richiesta HTTP e gestire la risposta. Dopodiché, è necessario creare un record di dati contenente l'URL del server, il metodo HTTP desiderato e le credenziali di autenticazione. Di seguito un esempio di codice:

```Elm
import Http
import Json.Decode as Decode

type alias AuthRequest =
    { url : String
    , method : Http.Method
    , username : String
    , password : String
    }

```

Una volta creato il record di dati, si può inviare la richiesta utilizzando la funzione `Http.send` passando come argomenti il metodo HTTP, l'URL del server, un decoder per gestire la risposta e i parametri della richiesta. Ecco un esempio:

```Elm
request : AuthRequest
request =
    { url = "https://example.com"
    , method = Http.get
    , username = "username"
    , password = "password"
    }

sendRequest : Cmd msg
sendRequest =
    Http.send AuthDecoder (Http.request request.url
        |> Http.withMethod request.method
        |> Http.withBasicAuth request.username request.password
    )

decodeResponse : Decoder msg
decodeResponse = 
    Decode.map AuthResponse
        (Decode.field "status_code" Decode.int)

```

Nell'esempio di codice sopra, si crea una richiesta con il metodo GET all'URL specificato, insieme alle credenziali di autenticazione fornite nel record `request`. Successivamente si invia la richiesta utilizzando la funzione `Http.send`. Infine, si decodifica la risposta utilizzando il decoder `AuthDecoder`, che deve essere definito in base alla struttura della risposta del server.

## Approfondimento

È importante notare che il modulo `Http` in Elm non supporta direttamente la modalità di autenticazione di base, ma è possibile utilizzare la funzione `Http.withBasicAuth` per impostare manualmente l'header della richiesta con le credenziali di autenticazione in formato Base64. Inoltre, è possibile utilizzare il middleware `Http.ExtraAuth` per semplificare il processo di autenticazione di base.

See Also

- [Elm HTTP Package Documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm Guide - Making HTTP Requests](https://guide.elm-lang.org/effects/http.html)
- [Elm HTTP Example](https://elmprogramming.com/elm-0.19-http-tutorial.html)