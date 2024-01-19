---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Invio di una Richiesta HTTP con Bash

## Che Cosa & Perché?
L'invio di una richiesta HTTP è un modo per interagire con i server web. Gli sviluppatori lo fanno per recuperare dati, inviare dati, o comunicare con API.

## Come fare:
Utilizzeremo `curl`, uno strumento da riga di comando, per inviare richieste HTTP. Ecco un esempio di richiesta GET:

```Bash
curl http://esempio.com
```
 
E un esempio di richiesta POST:

```Bash
curl -d "param1=valore1&param2=valore2" -X POST http://esempio.com
```

L'output sarà la risposta del server.

## Approfondimento
`curl` è stato creato da Daniel Stenberg nel 1997. È diventato uno strumento estremamente popolare per le sue funzioni versatili. Ci sono alternative a `curl`, come `wget`, ma '`curl` supporta un maggior numero di protocolli e opzioni.

L'invio di una richiesta HTTP con `curl` è un processo semplice: `curl` trasforma il tuo input in una richiesta HTTP, l'invia al server specificato, e stampa la risposta.

## Vedi Anche
Per maggiori dettagli su `curl` e le richieste HTTP, puoi consultare:

1. Documentazione di `curl`: https://curl.se/docs/
2. Guida alle richieste HTTP: https://developer.mozilla.org/it/docs/Web/HTTP
3. API Rest: https://www.redhat.com/it/topics/api/what-is-a-rest-api

-- Fine articolo --