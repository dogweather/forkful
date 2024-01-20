---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mandare una Richiesta HTTP con Autenticazione di Base in Clojure

## Cos'è e perché? 
Mandare una richiesta HTTP con autenticazione di base è il processo di invio di richieste a un API/server web e di verifica dell'identità dell'utente. I programmatori la usano per gestire e garantire la sicurezza delle informazioni sensibili tra client e server.

## Come fare:
Ecco come puoi implementare questa funzionalità con `clj-http`, un popolare cliente HTTP per Clojure. Aggiungi `clj-http` al tuo `project.clj`:

```Clojure
[clj-http "3.12.2"]
```

Ecco come effettuare una richiesta GET con autenticazione di base:

```Clojure
(ns myclient.http
  (:require [clj-http.client :as client]))

(defn get-request []
  (client/get "http://example.com/api/resource" {:basic-auth ["username" "password"]}))
```

Il codice di stato e il corpo della risposta si possono recuperare così:

```Clojure
(defn get-response []
  (let [response (get-request)]
    {:status (get response :status)
     :body (get response :body)}))
```

## Approfondimento
In passato, si utilizzava la libreria Java `java.net` per effettuare richieste HTTP ma forniva un livello d'astrazione molto basso rispetto alle moderne librerie Clojure come `clj-http`. 

Una alternativa è `httpkit`, che è un client molto leggero. Pero` non supporta l'autenticazione di base nativamente e dovrebbe essere implementata manualmente. 

L'implementazione in `clj-http` converte username e password in un header "Authorization". Questo header contiene "Basic" seguito da un codice base64 di username:password. 

## Per Approfondire
Per maggiori informazioni, consulta le seguenti risorse:
- Documentazione di clj-http: [https://clj-http.org](https://clj-http.org)
- Specifica dell'autenticazione HTTP: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Un blog post utile su come lavorare con richieste HTTP in Clojure: [http://thegeez.net/tech_clojure_and_http.html](http://thegeez.net/tech_clojure_and_http.html)