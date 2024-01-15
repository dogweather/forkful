---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Elm: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti dover inviare una richiesta HTTP con autenticazione di base in Elm. Ad esempio, potresti dover accedere a un API di terze parti o autenticarti a un server per ottenere risorse protette.

## Come Fare
Per inviare una richiesta HTTP con autenticazione di base in Elm, devi prima importare il modulo `Http` e `Json.Decode`. Successivamente, puoi utilizzare la funzione`sendWithCredential`, fornendo l'URL, il metodo di richiesta e i dati di autenticazione. Ad esempio:

```Elm
import Http
import Json.Decode

Http.sendWithCredential
    { url = "https://example.com/api"
    , method = "GET"
    , headers = []
    , body = Http.emptyBody
    }
    (\_ -> Http.expectString (handleResponse))

handleResponse response =
    case response of
        Http.BadUrl url ->
            "Errore nell'URL " ++ url
        Http.Timeout ->
            "Timeout della richiesta"
        Http.NetworkError ->
            "Errore di rete"
        Http.BadStatus status ->
            "Errore con uno stato " ++ String.fromInt status ++ " della richiesta"
        Http.BadBody body ->
            "Errore con il corpo della risposta"

```

Questo esempio invierà una richiesta GET all'URL fornito e gestirà eventuali errori di connessione o risposte non valide. Per aggiungere autenticazione di base, è sufficiente fornire username e password come dati di autenticazione:

```Elm
Http.sendWithCredential
    { url = "https://example.com/api"
    , method = "GET"
    , headers = []
    , body = Http.emptyBody
    }
    (\_ -> Http.expectString (handleResponse))
    |> Http.withBasicAuth "username" "password"
```
L'output della funzione `handleResponse` dipenderà dal tipo di risposta che ci si aspetta di ricevere. In questo caso, abbiamo utilizzato la funzione `expectString`, che aspetta una stringa come risposta.

## Approfondimento
La basic authentication è un metodo semplice e comune per autenticare gli utenti in una richiesta HTTP. Quando si utilizza questo tipo di autenticazione, i dati di nome utente e password sono codificati in base64 e inclusi nell'header `Authorization` della richiesta. Tuttavia, questo metodo non è sicuro in quanto i dati sono facilmente leggibili nell'header della richiesta. È consigliato utilizzare un metodo di autenticazione più sicuro come il token Bearer o il JWT.

## Vedi Anche
- Documentazione ufficiale di Elm su HTTP: https://guide.elm-lang.org/architecture/effects/http.html
- Tutorial su come utilizzare l'autenticazione di base in Elm: https://dev.to/matsimitsu/elegant-http-authentication-in-elm-11ba