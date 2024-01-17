---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Clojure: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa e perché?

L'invio di una richiesta HTTP con l'autenticazione di base è un processo in cui un programma invia una richiesta a un server web utilizzando un codice di autenticazione. I programmatori lo fanno per garantire che solo gli utenti autorizzati possano accedere a risorse sensibili sul server.

## Come fare:

```
(clojure.repl/doc basic-authenticate) 
```

Ecco un esempio di codice che utilizza la funzione `basic-authenticate` per inviare una richiesta GET a un server con autenticazione di base:

```
(basic-authenticate "GET" "https://example.com" :username "username" :password "password")
```

Questo restituirà una risposta del server contenente i dati richiesti. In caso di errore di autenticazione, verrà restituito un errore.

## Approfondimento:

L'autenticazione di base è uno dei modi più semplici per implementare la sicurezza nelle richieste HTTP. Originariamente sviluppata per l'uso in reti non sicure, è ancora ampiamente utilizzata oggi. Tuttavia, a causa dei suoi limiti di sicurezza, è consigliabile utilizzare altri metodi più robusti come OAuth o JSON Web Tokens per proteggere le risorse sensibili.

## Vedi anche:

Per ulteriori informazioni su come implementare l'autenticazione di base in Clojure, puoi consultare il seguente articolo: [https://clojuredocs.org/clojure.core/basic-authenticate](https://clojuredocs.org/clojure.core/basic-authenticate). Inoltre, puoi esplorare altre alternative come OAuth e JSON Web Tokens per comprendere meglio la sicurezza delle richieste HTTP.