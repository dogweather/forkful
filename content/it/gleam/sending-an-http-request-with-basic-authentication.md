---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Gleam: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler inviare una richiesta HTTP con l'autenticazione di base. Ad esempio, potresti essere un sviluppatore che lavora su un'API e hai bisogno di autenticare gli utenti per gestire le autorizzazioni. Oppure potresti essere un utente che sta cercando di accedere a un sito che richiede un login per accedere ai contenuti.

## How To
Per inviare una richiesta HTTP con l'autenticazione di base in Gleam, è necessario utilizzare la funzione `Request.basic_auth` e fornire le credenziali necessarie come parametri. Ecco un esempio di codice che illustra come farlo:

```Gleam
let username = "utente"
let password = "password"
let request = Request.basic_auth("http://www.esempio.com/api", username, password)
```

Il codice sopra creerà una richiesta con l'URL specificato e includerà le credenziali di autenticazione nella sua intestazione.

Per visualizzare l'output della richiesta, è possibile utilizzare la funzione `Response.body_text` per estrarre il corpo della risposta come testo. Ecco un esempio completo di come inviare una richiesta con l'autenticazione di base e stampare il risultato:

```Gleam
let username = "utente"
let password = "password"
let request = Request.basic_auth("http://www.esempio.com/api", username, password)
let response = Http.send(request)
let body = Response.body_text(response)
Log.info(body)
```

Questo codice invierà una richiesta al sito `www.esempio.com` con le credenziali di autenticazione fornite e stamperà il testo della risposta.

## Deep Dive
L'autenticazione di base è uno dei metodi più semplici per autenticare una richiesta HTTP. Consiste nell'inviare le credenziali (generalmente nome utente e password) come stringhe nella richiesta. Tuttavia, questo metodo non è sicuro poiché le credenziali sono inviate in chiaro e possono essere intercettate da terzi.

Per aumentare la sicurezza, è possibile utilizzare la codifica Base64 per codificare le credenziali prima di inviarle nella richiesta. In questo modo, le credenziali saranno meno leggibili e più difficili da intercettare.

Inoltre, è importante notare che l'autenticazione di base non supporta il concetto di sessioni. Ogni richiesta deve contenere le credenziali per essere autenticata, quindi non c'è un sistema di autenticazione persistente come per le sessioni.

## See Also
- [Documentazione ufficiale di Gleam su richieste HTTP](https://gleam.run/modules/http.html)
- [Articolo su autenticazione di base su Wikipedia](https://it.wikipedia.org/wiki/Basic_access_authentication)