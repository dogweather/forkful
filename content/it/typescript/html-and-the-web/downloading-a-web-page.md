---
title:                "Scaricare una pagina web"
aliases:
- it/typescript/downloading-a-web-page.md
date:                  2024-01-20T17:45:02.267495-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa acquisire il suo contenuto HTML per poterlo elaborare o analizzare. I programmatori lo fanno per raccogliere dati, testare la disponibilità di siti o interagire con servizi web.

## How to:
Ecco un esempio base usando `axios`, una libreria HTTP popolare.

```TypeScript
import axios from 'axios';

async function downloadPage(url: string): Promise<string> {
    try {
        const response = await axios.get(url);
        return response.data;
    } catch (error) {
        console.error('Errore durante il download della pagina:', error);
        throw error;
    }
}

// Utilizzo della funzione
const URL_DA_SCARICARE = 'https://example.com';
downloadPage(URL_DA_SCARICARE)
    .then(contenuto => {
        console.log(contenuto);
        // Qui puoi fare quello che vuoi con il contenuto HTML
    });
```

## Deep Dive

**Storia**: Nei primi giorni del web, scaricare una pagina era questione di una semplice richiesta GET HTTP. Oggi, le pagine sono spesso costruite lato client con JavaScript, rendendo il processo un po' più complicato.

**Alternative**: Altre librerie come `node-fetch` o il modulo `http` nativo di Node.js possono essere usate per scaricare pagine web.

**Dettagli Implementativi**: Quando usi `axios`, la gestione degli errori è importante per gestire risposte non valide o problemi di rete. `axios` gestisce sia le promesse che la sintassi async/await, il che lo rende molto comodo per operazioni asincrone.

## See Also

- Documentazione Axios: [https://axios-http.com/](https://axios-http.com/)
- node-fetch, una leggera alternativa a base di window.fetch per Node.js: [https://github.com/node-fetch/node-fetch](https://github.com/node-fetch/node-fetch)
- Guida HTTP di MDN per approfondire come funzionano le richieste web: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
