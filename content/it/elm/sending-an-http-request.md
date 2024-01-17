---
title:                "Inviare una richiesta http"
html_title:           "Elm: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

Cosa & Perché?

In poche parole, inviare una richiesta HTTP significa comunicare con un server per ottenere o inviare dati tramite Internet. I programmatori spesso utilizzano questo meccanismo per ottenere risorse esterne o per permettere alle loro applicazioni di comunicare con altri sistemi.

Come si fa:

```Elm
import Http

Http.get {url = "https://www.example.com/api/users", expect = Http.expectJson decodeUsersList}

decodeUsersList: Http.Decoder (List User)
decodeUsersList = Http.list User.decodeUser

type alias User = {id: Int, name: String}

decodeUser: Http.Decoder User
decodeUser = 
    D.map2 User
        (D.field "id" D.int)
        (D.field "name" D.string)
```

Deep Dive:

Per comprendere meglio il concetto di invio di richieste HTTP, è utile conoscere il suo contesto storico. In passato, il protocollo HTTP era utilizzato principalmente per il recupero di file HTML, ma con l'avvento delle applicazioni web, è diventato uno strumento fondamentale per lo scambio di dati tra client e server. Esistono anche alternative come WebSocket e MQTT, ma HTTP è ancora ampiamente utilizzato per il suo supporto per una vasta gamma di operazioni.

Quando si invia una richiesta HTTP, bisogna tenere conto di diversi aspetti come il metodo di richiesta (ad esempio GET o POST), gli header e il body della richiesta. Inoltre, è importante gestire anche la gestione degli errori e la gestione delle risposte, utilizzando funzioni come `Http.expectJson` e `Http.expectString`.

See Also:
-Documentazione ufficiale di Elm: elm-lang.org/docs/http
-Esempi di codice su GitHub: github.com/elm/http
-Introduzione alle richieste HTTP con Elm: medium.com/@theburningmonk/elm-http-get-trace-step-down-ad6cd467cb5d