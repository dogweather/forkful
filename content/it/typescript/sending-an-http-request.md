---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Invio di richieste HTTP con TypeScript

## Che cosa e Perché?
Invio di una richiesta HTTP implica la comunicazione tra il tuo codice e un server web per ottenere o salvare dati. Fa parte delle operazioni CRUD (Create, Read, Update, Delete) che sono la colonna vertebrale per la gestione dei dati in qualsiasi applicazione.

## Come fare:
In TypeScript, puoi utilizzare la `fetch API` per inviare richieste HTTP. Ecco un esempio semplice:

```TypeScript
fetch('https://api.example.com/dati')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Errore:', error));
```

In questo esempio, stiamo eseguendo una richiesta GET per ottenere i dati da un endpoint. Ecco un esempio di output:

```TypeScript
{ "nome": "Mario Rossi", "ruolo": "sviluppatore front-end" }
```

## Approfondimento
Il protocollo HTTP è stato sviluppato da Tim Berners-Lee nel 1989 per permettere la comunicazione tra client e server su Internet. Prima di `fetch API`, l'oggetto `XMLHttpRequest` era comunemente usato per inviare richieste HTTP, ma `fetch API` è più flessibile e potente.

Alternativamente, potresti utilizzare librarie come `axios` che supporta sia i callback che le promesse. Inoltre, `axios` fornisce una più ampia compatibilità con i vecchi browser rispetto alla `fetch API`. Ecco un esempio di come potresti utilizzare `axios`:

```TypeScript
import axios from 'axios';

axios.get('https://api.example.com/dati')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error(error);
  });
```

## Vedi Anche
- [Fetch API su MDN](https://developer.mozilla.org/it/docs/Web/API/Fetch_API/Using_Fetch)
- [Axios su GitHub](https://github.com/axios/axios)
- [HTTP su Wikipedia](https://it.wikipedia.org/wiki/Hypertext_Transfer_Protocol)

Invio di richieste HTTP è un aspetto fondamentale della programmazione web. Esplora le risorse sopra per saperne di più!