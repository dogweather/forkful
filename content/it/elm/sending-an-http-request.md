---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

L'invio di una richiesta HTTP nel contesto della programmazione Elm è un modo per i client di comunicare con i server web. I programmatori lo fanno per ottenere dati da fonti esterne, inviare dati e interagire con API esterne.

## Come fare:

Per inviare una richiesta HTTP in Elm, usiamo il modulo `Http` che fornisce funzionalità per effettuare richieste HTTP a server remoti. Ecco un semplice esempio:

```Elm
import Http
import Json.Decode as Decode

type Msg = GotResponse (Result Http.Error String)

main : Program () Msg ()
main =
    Http.get
        { url = "https://api.example.com"
        , expect = Http.expectString GotResponse
        }
    |> Http.send identity
```

In questo esempio, inviamo una richiesta GET all'url specificato e ci aspettiamo una risposta come stringa. La risposta sarà avvolta in un `Result` e fornita al messaggio `GotResponse`. 

## Approfondimento

Storicamente, l'invio di richieste HTTP è stato un pilastro della programmazione per il web da quando è stata introdotta la spec HTTP. In Elm, l'invio di richieste HTTP passa attraverso la pipeline di effetti gestita dal runtime di Elm, garantendo un flusso di dati unidirezionale pulito e coerente.

Come alternative, ci sono anche librerie Elm di terze parti come `elm-http-builder` che forniscono un'API più espressiva per comporre richieste HTTP complesse.

A livello di implementazione, la richiesta HTTP in Elm è costruita in modo da garantire che ogni variazione possibile della risposta HTTP sia esplicitamente gestita dal codice, promuovendo così ottime pratiche di gestione degli errori.

## Vedi Anche

- Documentazione ufficiale Elm HTTP: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Costruire HTTP Requests con `elm-http-builder`: [https://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/](https://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/)
- Guida alla gestione degli errori in Elm: [https://guide.elm-lang.org/error_handling/](https://guide.elm-lang.org/error_handling/)