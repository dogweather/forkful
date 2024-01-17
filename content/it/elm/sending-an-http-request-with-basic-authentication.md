---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Elm: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
In Elm, inviare una richiesta HTTP con autenticazione di base significa comunicare con un server utilizzando un metodo di sicurezza standard per l'autenticazione. I programmatori lo fanno per poter accedere a risorse protette dal server, come ad esempio ottenere dati sensibili o effettuare operazioni che richiedono autenticazione.

## Come Fare:
Per inviare una richiesta HTTP con autenticazione di base in Elm, è necessario utilizzare la funzione `Http.send` e specificare i parametri corretti come metodo, URL e credenziali di autenticazione. Ecco un esempio di codice:

```Elm
import Http
import Json.Decode as Decode

-- Esempio di richiesta GET con autenticazione di base
request =
    Http.send
        { method = "GET"
        , headers = []
        , url = "https://www.example.com"
        , body = Http.emptyBody
        , expect = Http.expectString identity
        , timeout = Nothing
        , withCredentials = Just True  -- questo imposta le credenziali di autenticazione
        , crossOrigin = Nothing
        }
```

## Approfondimento:
L'autenticazione di base è uno dei metodi di sicurezza più antichi ed è ancora utilizzata in molti sistemi. Esistono anche altri metodi di autenticazione, come l'autenticazione a chiave pubblica e l'autenticazione basata su token, che offrono livelli di sicurezza più elevati. Inoltre, esistono diversi modi per implementare l'autenticazione di base, come il metodo di autenticazione digest che utilizza un hash di sicurezza.

## Vedi Anche:
Per ulteriori informazioni su come inviare richieste HTTP con autenticazione di base in Elm, puoi consultare la documentazione ufficiale di Elm su `Http.send`. Inoltre, ci sono diverse librerie di terze parti che offrono funzionalità aggiuntive per l'autenticazione di base in Elm, come ad esempio la libreria `elm-basic-auth`.