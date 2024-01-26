---
title:                "Inviare una richiesta http"
date:                  2024-01-20T18:00:53.527748-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Inviare una richiesta HTTP significa richiedere dati o effettuare azioni su un server remoto. I programmatori lo fanno per interagire con dati dinamici, servizi esterni o API, direttamente dalle loro applicazioni.

## How to: (Come Fare:)
```typescript
import axios from 'axios';

// GET Request per ottenere dati.
axios.get('https://api.exempio.com/data')
  .then(response => {
    console.log('Dati ricevuti:', response.data);
  })
  .catch(error => {
    console.error('Errore durante la richiesta:', error);
  });

// POST Request per inviare dati.
axios.post('https://api.exempio.com/submit', { nome: 'Mario', eta: 30 })
  .then(response => {
    console.log('Risposta dal server:', response.data);
  })
  .catch(error => {
    console.error('Errore durante il POST:', error);
  });
```

Esempio di output:
```
Dati ricevuti: { ... } // dettagli dei dati
Risposta dal server: { ... } // risposta del server
```

## Deep Dive (Approfondimento)
Le richieste HTTP sono la base della comunicazione su internet sin dagli anni '90. Il protocollo HTTP ha abilitato il web a crescere e a evolversi in quello che conosciamo oggi. Alternativamente, invece di `axios`, che è una libreria esterna, si potrebbe usare la API `fetch` nativa di JavaScript:

```typescript
fetch('https://api.exempio.com/data')
  .then(response => response.json())
  .then(data => console.log('Dati fetch:', data))
  .catch(error => console.error('Errore fetch:', error));
```

Però, `fetch` non gestisce automaticamente i cookie o un timeout di default, e di default non rifiuta le richieste HTTP che restituiscono un codice di stato di errore. `Axios` offre queste funzionalità e molte altre out-of-the-box, rendendola una scelta comune tra gli sviluppatori.

## See Also (Vedi Anche)
- Documentazione di Axios: https://axios-http.com/docs/intro
- Documentazione di fetch MDN: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- HTTP Cats: illustrazioni creative dei codici di stato HTTP: https://http.cat/
