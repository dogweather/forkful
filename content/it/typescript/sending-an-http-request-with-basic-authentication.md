---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

L'invio di una richiesta HTTP con autenticazione di base è un metodo mediante il quale i nostri programmi possono richiedere e comunicare dati da e verso i server web. I programmatori utilizzano questa tecnica per accedere a risorse protette, come API che necessitano di credenziali per l'accesso, in modo sicuro e semplice.

## Come fare:

Ecco un esempio su come inviare una richiesta HTTP con autenticazione di base utilizzando Axios, una libreria di TypeScript molto popolare:

```TypeScript
import axios from 'axios';

axios({
  method: 'get',
  url: 'http://your-url.com',
  auth: {
    username: 'username',
    password: 'password'
  }
})
  .then(response => {
    console.log(response);
  })
  .catch(error => {
    console.log(error);
  });
```

In questo esempio, 'username' e 'password' rappresentano le tue credenziali d'accesso. L'URL è dove vuoi inviare la richiesta.

## Approfondimento

Storicamente, l'autenticazione basica era un modo comune per gestire le autorizzazioni, perché è semplice da implementare. Tuttavia, trasmette le credenziali come testo non criptato (Base64), rendendolo sconsigliato per la produzione senza un protocollo HTTPS.

Un'alternativa all'autenticazione di base è l'autenticazione basata su token, come JWT (JSON Web Token). Mentre l'autenticazione di base richiede che le credenziali siano inviate con ogni richiesta, JWT invia un token di accesso, che può essere controllato e memorizzato più sicuro e facilmente per le successive richieste.

L'implementazione dell'autenticazione di base in TypeScript con Axios avviene configurando l'oggetto `auth` all'interno dell'oggetto `axios()`. Tieni presente che le tue credenziali verranno inviate con ogni richiesta HTTP, quindi assicurati di gestirle in modo sicuro.

## Vedi anche

- [Axios su Github](https://github.com/axios/axios)
- [Autenticazione HTTP su MDN (Mozilla Developer Network)](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
- [JWT (JSON Web Token) sito ufficiale](https://jwt.io/)