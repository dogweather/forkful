---
title:                "Scaricare una pagina web"
html_title:           "Elm: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Sai quando vuoi scaricare una pagina web per leggerla offline? O quando vuoi ottenere dati da un sito per elaborarli? Bene, la programmazione di Elm può aiutarti a farlo in modo semplice e veloce. Continua a leggere per scoprire come!

## Come fare

La prima cosa da fare è importare il modulo `Http` per effettuare richieste http. Possiamo farlo in questo modo:

```Elm
import Http
```

Poi dobbiamo definire la nostra richiesta http, specificando sia l'url che il metodo di richiesta (GET, POST, PUT, etc.). Ad esempio, se volessimo ottenere i dati di questa pagina web, possiamo scrivere:

```Elm
let
    request = Http.request
        { method = "GET"
        , url = "https://www.example.com"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Debug.log "Got response!")
        }
```

In questo caso, abbiamo definito una richiesta GET all'url `https://www.example.com`, impostato il corpo della richiesta a vuoto `Http.emptyBody` e specificato che ci aspettiamo una risposta di tipo stringa `expectStringResponse`. Utilizziamo anche una funzione di `Debug.log` per visualizzare nella console del browser quando otteniamo una risposta.

Infine, dobbiamo inviare effettivamente la richiesta utilizzando la funzione `send`:

```Elm
Http.send request
```

## Approfondimento

Se vuoi saperne di più su come funziona il download di una pagina web in Elm, puoi approfondire la documentazione del modulo `Http` e provare a sperimentare con vari tipi di richieste e risposte.

## Vedi anche

Ecco alcuni link utili per ulteriori informazioni sulla programmazione in Elm:

- Sito ufficiale di Elm: https://elm-lang.org/
- Documentazione del modulo Http: https://package.elm-lang.org/packages/elm/http/latest/
- Esempi di codice di Elm: https://github.com/elm-lang/examples