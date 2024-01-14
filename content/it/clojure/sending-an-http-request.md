---
title:                "Clojure: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

In molti casi, ci può essere la necessità di inviare una richiesta HTTP a un server per ottenere dati o informazioni. Questo può essere fatto utilizzando il linguaggio di programmazione Clojure, che offre strumenti semplici ed efficienti per inviare richieste HTTP.

## Come Fare

Per inviare una richiesta HTTP in Clojure, è necessario utilizzare una libreria esterna chiamata `clj-http`. La prima cosa da fare è importare questa libreria nel tuo progetto.

```
(require '[clj-http.client :as client])
```

Una volta importata, puoi utilizzare la funzione `client/get` per eseguire una richiesta GET ad un URL specifico. Ad esempio, se volessi ottenere il contenuto di una pagina web, puoi fare così:

```
(client/get "https://www.example.com")
```

Questa richiesta restituirà un valore che contiene tutte le informazioni sulle intestazioni, il corpo della risposta e codice di stato.

Puoi anche inviare una richiesta POST utilizzando la funzione `client/post`. Ad esempio, se volessi inviare dei dati ad un server attraverso una richiesta POST, puoi farlo in questo modo:

```
(client/post "https://www.example.com/user" {:form-params {:username "Mario" :password "password123"}})
```

In questo esempio, stiamo inviando un oggetto di dati nel corpo della richiesta, che verrà automaticamente codificato in una stringa di parametri formattati.

## Approfondimenti

Ci sono molti altri parametri che puoi utilizzare per personalizzare la tua richiesta, come ad esempio l'aggiunta di intestazioni specifiche o l'utilizzo di autenticazione. Puoi trovare ulteriori informazioni sulla libreria `clj-http` nella documentazione ufficiale, che include anche esempi di codice per utilizzare queste funzioni in modo più avanzato.

## Vedi Anche

- Documentazione ufficiale della libreria "clj-http": https://github.com/dakrone/clj-http
- Tutorial su come inviare una richiesta HTTP in Clojure: https://stackify.com/clojure-tutorial-rest-client/