---
title:                "Inviare una richiesta http"
html_title:           "Gleam: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Web, probabilmente hai incontrato l'uso delle richieste HTTP. In Gleam, puoi inviare facilmente richieste HTTP usando la libreria `httpc`.

## Come

Per inviare una richiesta HTTP, dovrai prima importare la libreria `httpc` nel tuo codice Gleam.

```Gleam
import httpc
```

Dopodiché, puoi utilizzare la funzione `request` per inviare la richiesta. Ad esempio, per inviare una richiesta GET, puoi usare il seguente codice:

```Gleam
let response = httpc.request(
  url = "https://api.github.com/users/gleam-lang",
  method = httpc.GET
)
```

In questo esempio, stiamo inviando una richiesta GET all'API di GitHub per ottenere informazioni sul profilo di Gleam.

Puoi anche specificare dei parametri nella tua richiesta usando il campo `params` come un record che mappa i parametri alle loro rispettive chiavi. Ad esempio:

```Gleam
let response = httpc.request(
  url = "https://api.github.com/search/repositories",
  method = httpc.GET,
  params = { q: "language:\"Gleam\"" }
)
```

Questo ci permette di fare una ricerca dei repository GitHub che hanno il linguaggio "Gleam". Ovviamente, puoi cambiare la chiave e il valore dei tuoi parametri a seconda delle tue esigenze.

Per inviare una richiesta POST, invece, puoi utilizzare il campo `body` che accetta una stringa contenente i dati che vuoi inviare. Ad esempio:

```Gleam
let response = httpc.request(
  url = "https://httpbin.org/post",
  method = httpc.POST,
  body = "Hello World!"
)
```

In questo caso, stiamo inviando una richiesta POST al sito di test `httpbin.org` con un semplice messaggio di testo come corpo della richiesta.

Una volta inviata la richiesta, puoi accedere alla risposta utilizzando i metodi della libreria `httpc` come `response.status`, `response.headers` e `response.body`.

## Deep Dive

La libreria `httpc` in realtà è un "wrapping" intorno alla libreria Erlang `:httpc`, che fornisce funzionalità HTTP di più basso livello. Quindi, se sei un utente più esperto, puoi anche utilizzare direttamente questa libreria Erlang.

Inoltre, puoi specificare altre opzioni nella tua richiesta, come autenticazione, dati di tipo JSON e molto altro ancora. Per saperne di più, puoi consultare la documentazione della libreria `httpc` [qui](https://hexdocs.pm/gleam/httpc.html).

## Vedi anche

- [Documentazione ufficiale di Gleam](https://gleam.run/)
- [Esempio di codice di richiesta HTTP in Gleam](https://github.com/gleam-lang/gleam/blob/master/lib/httpc/examples/basic_request.gleam)
- [Documentazione ufficiale di libreria `httpc`](https://hexdocs.pm/gleam/httpc.html)