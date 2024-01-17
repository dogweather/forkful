---
title:                "Inviare una richiesta http"
html_title:           "Clojure: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
In breve, inviare una richiesta HTTP significa comunicare con un server web per ottenere una risorsa, come una pagina web o un file. I programmatori fanno questo per accedere a dati o funzionalità dal server in modo che possano essere utilizzati nel proprio codice.

## Come fare:
```Clojure
(ns myapp.core
  (:require [clojure.http.client :as http]))

; Esempio di invio di una richiesta GET al server
(http/get "https://www.amazon.com")

; Esempio di invio di una richiesta POST con dati aggiuntivi
(http/post "https://www.example.com/login" {:username "user123" :password "pa$$word"})

```

## Approfondimenti:
In passato, per interagire con i server era necessario utilizzare protocolli più complessi come FTP o Telnet. Tuttavia, oggi l'utilizzo di richieste HTTP è diventato lo standard de facto per la comunicazione con i server web. In alternativa, i programmatori possono utilizzare librerie esterne per semplificare le richieste HTTP, come HTTP Client incluso nella libreria standard di Clojure.

## Vedi anche:
- [HTTP Client: Libreria standard di Clojure](https://clojure.github.io/clojure/clojure.http.client-api.html)
- [How to Use HTTP Requests in Clojure](https://www.stuartsierra.com/2014/01/25/how-to-use-http-requests-in-clojure/)