---
title:                "TypeScript: Invio di una richiesta http"
simple_title:         "Invio di una richiesta http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando sviluppiamo un'applicazione, abbiamo bisogno di scambiare dati con un server. In questi casi, ci affidiamo alle richieste HTTP per inviare e ricevere informazioni. Le richieste HTTP sono fondamentali per la comunicazione tra un client e un server, quindi è importante saperle utilizzare nel modo corretto.

## Come fare

Per inviare una richiesta HTTP in TypeScript, possiamo utilizzare la libreria axios. Per prima cosa, dobbiamo installarla nel nostro progetto utilizzando il comando npm install axios --save. Una volta installata, possiamo importarla nel nostro codice TypeScript e utilizzarla per effettuare la richiesta. Ad esempio, se vogliamo ottenere i dati da un endpoint API, possiamo utilizzare il metodo GET di axios nel seguente modo:

```TypeScript
const axios = require('axios');

axios.get('https://api.example.com/users')
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.log(error);
})
```

In questo caso, stiamo inviando una richiesta GET all'endpoint "https://api.example.com/users" e stampando i dati ricevuti dalla risposta nella console. Possiamo anche aggiungere parametri alla nostra richiesta, come ad esempio un token di autorizzazione o dei dati da inviare al server.

## Deep Dive

Le richieste HTTP sono costituite da una serie di elementi fondamentali, come il metodo HTTP utilizzato (GET, POST, PUT, DELETE, ecc.), l'endpoint al quale inviamo la richiesta e gli eventuali dati aggiuntivi. Inoltre, possiamo anche specificare degli header nella nostra richiesta, che contengono informazioni aggiuntive come ad esempio il tipo di contenuto o il token di autorizzazione. Anche la risposta ricevuta dal server contiene degli header che ci possono fornire informazioni utili, ad esempio il codice di stato della risposta (200 per una richiesta andata a buon fine), il tipo di contenuto o il length dei dati ricevuti.

È importante anche gestire gli errori nelle nostre richieste HTTP in modo corretto, utilizzando il metodo .catch di axios per gestire eventuali problemi con la richiesta o con la risposta del server.

## Vedi Anche

- [Documentazione di axios](https://www.npmjs.com/package/axios)
- [Esempi di richieste HTTP con TypeScript](https://www.carlrippon.com/creating-direct-line-client-in-typescript/)
- [Guida alle richieste HTTP in Express.js](https://expressjs.com/it/guide/routing.html)