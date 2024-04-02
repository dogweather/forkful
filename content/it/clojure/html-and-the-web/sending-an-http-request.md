---
date: 2024-01-20 17:59:12.035806-07:00
description: "Inviare una richiesta HTTP \xE8 il modo con cui il tuo programma chiede\
  \ dati o invia informazioni a un server web. I programmatori lo fanno per interagire\u2026"
lastmod: '2024-03-13T22:44:43.039090-06:00'
model: gpt-4-1106-preview
summary: "Inviare una richiesta HTTP \xE8 il modo con cui il tuo programma chiede\
  \ dati o invia informazioni a un server web. I programmatori lo fanno per interagire\u2026"
title: Inviare una richiesta http
weight: 44
---

## What & Why?
Inviare una richiesta HTTP è il modo con cui il tuo programma chiede dati o invia informazioni a un server web. I programmatori lo fanno per interagire con servizi web, consumare API o servire contenuti agli utenti.

## How to:
In Clojure, puoi usare la libreria `clj-http` per fare richieste HTTP facilmente.

```Clojure
(require '[clj-http.client :as client])

; GET Request
(def response (client/get "https://jsonplaceholder.typicode.com/posts/1"))
(println response)

; POST Request con body in JSON
(def post-response 
  (client/post "https://jsonplaceholder.typicode.com/posts"
               {:headers {"Content-Type" "application/json"}
                :body (json/write-str {:title "Clojure" :body "Article" :userId 1})}))
(println post-response)
```

L'output sarà un mappa Clojure con status, headers, e body della risposta, dipendendo dal tipo di richiesta fatta.

## Deep Dive:
Prima delle librerie come `clj-http`, mandare richieste HTTP in Clojure richiedeva più passaggi e meno astrazioni, tipicamente usando Java interop. `clj-http` incapsula queste complessità rendendo il processo più semplice e idiomatico per Clojure.

Alternatives a `clj-http` includono `http-kit` e `aleph`, ognuna con le proprie peculiarità in termini di performance e stile di programmazione.

Per quanto riguarda l'implementazione, `clj-http` utilizza Apache HttpClient sotto il cappuccio, una libreria Java collaudata e affidabile. Gestisce anche connessioni HTTPS, reindirizzamenti, cookie e molte altre features HTTP.

## See Also:
- [clj-http GitHub repository](https://github.com/dakrone/clj-http)
- [Clojure - Getting Started](https://clojure.org/guides/getting_started)
- [JSONPlaceholder REST API](https://jsonplaceholder.typicode.com/)
