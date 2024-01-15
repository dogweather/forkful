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

## Perché

Ci sono molti motivi per cui potresti voler inviare una richiesta HTTP in Clojure. Ad esempio, potresti aver bisogno di comunicare con un server per ottenere dati, inviare informazioni di registrazione o semplicemente esplorare e imparare come funziona la comunicazione tramite richieste HTTP.

## Come

Ecco un semplice esempio di come inviare una richiesta HTTP in Clojure utilizzando la libreria `clj-http`:

```Clojure
(ns http-example.core
  (:require [clj-http.client :as client]))

(defn send-request []
  (let [response (client/get "https://jsonplaceholder.typicode.com/todos/1")]
    (println (:status response)) ; Stampa lo stato della risposta
    (println (:body response))))) ; Stampa il corpo della risposta
```

L'output di questo codice sarà:

```
200
{"userId": 1, "id": 1, "title": "delectus aut autem", "completed": false}
```

Per inviare una richiesta in un formato diverso da `GET`, puoi utilizzare la funzione `post`. Ad esempio:

```Clojure
(client/post "https://jsonplaceholder.typicode.com/posts"
             {:body {:title "Test post" :body "This is a test post"}})
```

## Deep Dive

Invio di richieste con `clj-http` è molto semplice, tuttavia, se vuoi esplorare più opzioni e comprendere meglio come funziona la comunicazione HTTP, ci sono alcuni concetti importanti da considerare:

- È possibile specificare header personalizzati utilizzando il parametro `:headers` nelle funzioni `get` e `post`.
- Puoi passare parametri di query utilizzando il parametro `:params`.
- Puoi specificare diversi tipi di contenuti utilizzando il parametro `:content-type`.

Per ulteriori informazioni, puoi consultare la documentazione ufficiale di `clj-http`.

## Vedi anche

- [Documentazione ufficiale di `clj-http`](https://github.com/dakrone/clj-http)
- [Tutorial su richieste HTTP in Clojure](https://www.luminusweb.net/docs/http-client.html)
- [Esempio di gestione di richieste HTTP con `clj-http`](https://www.hackingnote.com/en/clojure-alexandria-clj-http/)