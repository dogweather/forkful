---
title:                "TypeScript: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

La comunicazione tra un client e un server tramite il protocollo HTTP è fondamentale per lo sviluppo di applicazioni web moderne. Tuttavia, è importante garantire che gli accessi ai dati siano sicuri e protetti da eventuali attacchi esterni. Ecco perché l'autenticazione di base (basic authentication) viene utilizzata per autorizzare l'accesso ai dati da parte del client al server.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in TypeScript, è necessario importare il modulo "http" e utilizzare la funzione "request" per creare una nuova richiesta. Passiamo i parametri del metodo "request" come oggetto, specificando il metodo HTTP desiderato, l'URL e le informazioni di autenticazione.

```
TypeScript
import * as http from 'http';

http.request({
  method: 'GET',
  url: 'http://www.example.com',
  auth: 'username:password'
}, (response) => {
  let data = '';

  response.on('data', (chunk) => {
    data += chunk;
  });

  response.on('end', () => {
    console.log(data);
  });
}).end();
```

L'output di questa richiesta sarà il contenuto della pagina all'URL specificato.

## Approfondimento

Di solito, il valore dell'autenticazione di base viene convertito in una stringa codificata in Base64 prima di essere trasmessa sulla rete. Possiamo fare lo stesso utilizzando il metodo "Buffer" di Node.js e specificando il tipo di codifica come parametro.

```
TypeScript
import * as http from 'http';
import * as Buffer from 'buffer';

let credentials = Buffer.Buffer.from('username:password').toString('base64');

http.request({
  method: 'GET',
  url: 'http://www.example.com',
  headers: {
    'Authorization': `Basic ${credentials}`
  }
}, (response) => {
  let data = '';

  response.on('data', (chunk) => {
    data += chunk;
  });

  response.on('end', () => {
    console.log(data);
  });
}).end();
```

Inoltre, ricordarsi di gestire gli errori e le eccezioni durante l'invio e la ricezione delle richieste HTTP, per garantire un'applicazione robusta e stabile.

## Vedi anche

- [Documentazione ufficiale su HTTP in Node.js](https://nodejs.org/api/http.html)
- [Esempi di codice TypeScript per l'utilizzo di HTTP con autenticazione](https://github.com/request/request/tree/master/examples)