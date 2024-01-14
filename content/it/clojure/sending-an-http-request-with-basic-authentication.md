---
title:                "Clojure: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori utilizzano le richieste HTTP con autenticazione di base per garantire la sicurezza delle comunicazioni tra un client e un server. Questo è particolarmente utile quando si tratta di scambiare dati sensibili come informazioni di login o pagamenti. In questo post, esploreremo come implementare una richiesta HTTP con autenticazione di base in Clojure.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base, abbiamo bisogno di tre elementi: l'URL del server, le credenziali (username e password) e i dati che desideriamo inviare. Utilizziamo la libreria "http-kit" che offre una semplice interfaccia per creare richieste HTTP in Clojure.

```
(ns my-app.core
 (:require [org.httpkit.client :as http]))

(def url "http://example.com/api")
(def username "myusername")
(def password "mypassword")
(def data {:name "John Doe" :age 30})

(defn send-request []
  (let [response (http/post url
                    {:basic-auth [username password]
                     :body (json-encode data)})
    (prn (http/status response))
    (prn (http/headers response))
    (prn (http/body response))
    )
  )
```

Nell'esempio sopra, stiamo inviando una richiesta POST all'URL specificato, con le credenziali fornite e i dati nella forma di un oggetto Clojure. Utilizzando la funzione "json-encode", convertiamo i dati in formato JSON prima di inviarli al server. La funzione "prn" ci permette di stampare il codice di stato, le intestazioni e il corpo della risposta ricevuta dal server.

## Approfondimento

Ora che sappiamo come inviare una richiesta HTTP con autenticazione di base in Clojure, potremmo voler capire meglio come funziona il meccanismo di autenticazione. Quando viene inviata una richiesta con autenticazione di base, il client codifica le credenziali (username e password) in base64 e le inserisce nell'intestazione "Authorization". Questo metodo di autenticazione non è considerato sicuro poiché le credenziali sono inviate in chiaro, quindi è consigliato utilizzarlo solo su connessioni HTTPS.

## Vedi anche

- Documentazione di "http-kit": https://github.com/http-kit/http-kit
- Guida di Clojure per la creazione di richieste HTTP: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/http-post
- Spiegazione dell'autenticazione di base: https://tools.ietf.org/html/rfc2617