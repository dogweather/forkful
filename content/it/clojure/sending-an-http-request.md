---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Richiedere HTTP con Clojure

## Che Cos'è & Perché?

L'invio di una richiesta HTTP è il processo di contatto e richiesta di dati da un server remoto. È fondamentale per gli sviluppatori creare applicazioni web interattive e connesse al web.

## Come Fare:

Usiamo `clj-http`, una comoda libreria Clojure per fare richieste HTTP. Inizia aggiungendo `clj-http` al tuo `project.clj`

```Clojure
(defproject your-project "0.1.0-SNAPSHOT"
 :dependencies [[org.clojure/clojure "1.10.1"]
                [clj-http "3.10.1"]]) 
```

E ora, un esempio di GET request:

```Clojure
(require '[clj-http.client :as client])
(let [response (client/get "http://example.com")])
  (println (:status response))
  (println (:body response)))
```

Nell'esempio, la richiesta GET restituirà lo status HTTP e il body della risposta.

## Una Capitombolatura

`clj-http` è ampiamente utilizzato in Clojure per molte ragioni come le sue potenti funzionalità e la facilità d'uso. Tuttavia, non è l'unico - esistono alternative come `http-kit` che potrebbe essere più veloce in alcuni casi.

Historicamente, le richieste HTTP in Clojure sono diventate più facili e veloci negli ultimi anni con l'avvento di librerie come `clj-http`.

## Per Saperne di Piu

Ecco alcuni link utili per ulteriori dettagli e risorse correlate sulle richieste HTTP in Clojure. 

1. [Documentazione ufficiale clj-http](https://github.com/dakrone/clj-http)
2. [Documentazione ufficiale http-kit](http://www.http-kit.org/)

Felice programmazione in Clojure!