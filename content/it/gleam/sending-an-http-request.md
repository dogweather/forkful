---
title:                "Inviare una richiesta http"
html_title:           "Gleam: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

Cosa & Perché?

In poche parole, inviare una richiesta HTTP significa trasmettere informazioni a un server web e richiedere una risposta. I programmatori lo fanno per interagire con le API, recuperare dati da un server o eseguire operazioni di base come l'accesso a una pagina web.

Come fare:

Gleam semplifica l'invio di richieste HTTP grazie alla sua libreria integrata "httpc". Di seguito un esempio che utilizza "httpc.get" per inviare una richiesta GET e ottenere una risposta da un server:

```Gleam
import httpc

let url = "https://swapi.dev/api/people/1/"
let response = httpc.get(url)
```

Il risultato sarà un record contenente la risposta del server, come ad esempio: ```some({ statusCode: 200, body: "{\"name\":\"Luke Skywalker\",\"height\":\"172\",\"birth_year\":\"19BBY\",\"films\":[\"https://swapi.dev/api/films/1/\",\"https://swapi.dev/api/films/2/\",\"https://swapi.dev/api/films/3/\",\"https://swapi.dev/api/films/6/\",\"https://swapi.dev/api/films/7/\"]}")```

Deep Dive:

Le richieste HTTP sono state introdotte nel 1991 e hanno da allora rivoluzionato il modo in cui interagiamo con i server web. Al giorno d'oggi esistono diverse alternative per inviare una richiesta HTTP, come l'utilizzo di librerie di terze parti o l'utilizzo diretto delle API di sistema del sistema operativo. La libreria "httpc" di Gleam utilizza la libreria "libcurl" per l'implementazione delle richieste.

Vedi anche:

- Documentazione ufficiale Gleam su "httpc": http://gleam.run/modules/httpc
- "What is an HTTP request?": https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- "libcurl": https://curl.se/libcurl/