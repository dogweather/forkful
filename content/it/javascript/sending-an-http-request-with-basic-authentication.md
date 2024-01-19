---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

L'invio di una richiesta HTTP con autenticazione di base è un processo in cui un client invia una richiesta a un server chiedendo l'accesso a risorse proteggiate. I programmatori lo fanno per proteggere le risorse da accessi non autorizzati e per mantenere l'integrità dei dati.

## Come si Fa:
Per inviare una richiesta HTTP con autenticazione di base in Javascript, puoi utilizzare la libreria `axios`. 

Ecco un esempio:

```Javascript
const axios = require('axios');
const username = 'username';
const password = 'password';

axios({
  method: 'get',
  url: 'https://mywebsite.com/',
  auth: {
    username: username,
    password: password
  }
})
.then(response => {
  console.log(response);
})
.catch(error => {
  console.log(error);
});
```
Quando esegui questo script, vedrai l'output della risposta del server o l'errore nel tuo terminale.

## Andiamo Più a Fondo
L'autenticazione HTTP di base esiste da molto tempo e fa parte delle specifiche HTTP originali. Offre un modo veloce e semplice per proteggere le risorse, ma non è molto sicura, dato che le credenziali vengono inviate come stringa di testo non criptata.

Come alternativa, potresti considerare l'uso di token JWT (JSON Web Token) o l'OAuth, che offre un maggiore livello di sicurezza.

Per quanto riguarda i dettagli dell'implementazione, `axios` codifica semplicemente le tue credenziali in base64 e le include nell'header della richiesta. Il server decodifica l'header, verifica le credenziali e risponde con le risorse richieste o con uno status di errore.

## Vedi Anche
Per saperne di più su questo argomento, dai un'occhiata ai seguenti link:

1. Documentazione di axios - https://axios-http.com/docs/intro
2. Autenticazione HTTP di base su MDN - https://developer.mozilla.org/it/docs/Web/HTTP/Authentication
3. Alternativa all'autenticazione di base, Auth0 - https://auth0.com/