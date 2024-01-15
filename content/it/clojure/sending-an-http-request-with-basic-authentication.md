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

## Perché

Molte volte, quando si sviluppano applicazioni Web, è necessario autenticare gli utenti per garantire che solo coloro con le credenziali corrette possano accedere a risorse protette. In questo articolo, vedremo come possiamo inviare una richiesta HTTP con autenticazione di base in Clojure.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Clojure, possiamo utilizzare la libreria `clj-http`. Per prima cosa, dobbiamo importare la libreria nel nostro progetto:

```Clojure
(ns tuo-progetto.core
  (:require [clj-http.client :as client]))
```

Successivamente, possiamo utilizzare la funzione `client/get` per inviare la richiesta con l'autenticazione di base. Dobbiamo passare l'URL della risorsa protetta, le credenziali dell'utente e l'header che indica l'utilizzo dell'autenticazione di base:

```Clojure
(client/get "https://www.example.com/restricted-resource"
             {:basic-auth ["utente" "password"]
              :headers {"Authorization" (str "Basic " (b64-encode (str "utente:password"))})})
```

L'output della richiesta sarà un hash con le informazioni della risposta, come ad esempio lo status code e il corpo della risorsa.

## Approfondimento

L'autenticazione di base è uno dei metodi più semplici per autenticare gli utenti in una applicazione Web. Funziona inserendo le credenziali dell'utente all'interno dell'header "Authorization" della richiesta HTTP, codificate in base64.

Un'altra opzione per implementare l'autenticazione di base in Clojure è utilizzare la libreria `ring` e il suo middleware `basic-auth`. Questo ci consente di proteggere rotte specifiche della nostra applicazione utilizzando l'autenticazione di base.

## Vedi anche

- Documentazione di `clj-http`: https://github.com/dakrone/clj-http
- Esempi di utilizzo di `basic-auth` con `ring`: https://github.com/ring-clojure/ring/wiki/Session-middleware#basic-authentication