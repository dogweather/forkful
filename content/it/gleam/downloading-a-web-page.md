---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Scarica una pagina web con Gleam

## Che cosa e perché?
Scaricare una pagina web significa recuperarlo dal server web su cui è ospitata. I programmatori fanno questo per vari motivi, tra cui l'analisi dei dati, il controllo delle modifiche e l'automazione dei compiti sulla web.

## Come fare:
Ecco un esempio di come scaricare una pagina web usando la libreria `gleam/httpc` in Gleam. 
Assicurati di aver aggiunto "gleam/httpc" al tuo file `rebar.config`.

```gleam
import gleam/httpc
import gleam/http.{Request}

fn scarica_pagina() {
  let req = Request(url: "https://example.com") 
  let _ = httpc.send(req)
}
```

Quando esegui questo codice, invierai una richiesta GET a "https://example.com" e riceverai una risposta dal server.

## Approfondimento
Il download di pagine web è una pratica che risale alla nascita del web stesso. Inizialmente, il download di una pagina web era un'operazione semplice: si inviava una richiesta GET a un server e si riceveva il codice HTML della pagina. Oggi, molte pagine web sono dinamiche e richiedono un numero più complesso di passaggi per essere scaricate completamente.

Ci sono molte librerie e strumenti alternativi per scaricare pagine web in diverse lingue e piattaforme. Alcuni popolari includono `requests` in Python, `axios` in Javascript, e `HttpClient` in .NET. 

`gleam/httpc` è una libreria di Gleam che rende semplice inviare richieste HTTP. La sua implementazione è un wrapper intorno all'implementazione Erlang di httpc, che è molto affidabile e ben supportata.

## Vedi anche
Per saperne di più su come utilizzare la libreria `gleam/httpc`, consulta la [documentazione ufficiale](https://hexdocs.pm/httpc/readme.html).

Per un'introduzione più dettagliata al Gleam, potrebbe essere utile la [Guida di introduzione a Gleam](https://gleam.run/getting-started/).