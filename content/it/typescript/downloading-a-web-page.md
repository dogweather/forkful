---
title:                "Scaricare una pagina web"
html_title:           "TypeScript: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scaricare una pagina web significa ottenere il suo contenuto e visualizzarlo sul tuo dispositivo. I programmatori lo fanno per creare applicazioni web, che devono ottenere i dati da diverse fonti per funzionare correttamente.

## Come fare:

Provalo tu stesso:

```TypeScript
// Importa il modulo HTTP
import * as http from "http";

// Crea un'istanza dell'oggetto HTTP
let request = http.request("https://www.esempio.com/", (response) => {
  // Gestisci il contenuto della risposta
  console.log(response.statusCode);
});

// Invia la richiesta
request.end();
```

Ecco il risultato della tua richiesta:

```TypeScript
200
```

## Approfondimento:

Scaricare una pagina web è una funzionalità fondamentale per le applicazioni web moderne. Prima dello sviluppo di linguaggi di programmazione come TypeScript, i programmatori dovevano scrivere codice più complesso per ottenere una pagina web. Oggi, ci sono molte alternative per scaricare una pagina web, come ad esempio l'utilizzo di librerie di terze parti come Axios o Fetch. Per implementare il download di una pagina web, i programmatori devono conoscere il protocollo HTTP e il funzionamento delle richieste e delle risposte.

## Vedi anche:

- https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- https://www.npmjs.com/package/axios
- https://www.npmjs.com/package/node-fetch