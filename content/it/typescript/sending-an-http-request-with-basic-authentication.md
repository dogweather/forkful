---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "TypeScript: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

C'è molta attenzione sui sistemi di autenticazione avanzati come OAuth, ma a volte ci sono situazioni in cui l'utilizzo di un semplice sistema di autenticazione di base è sufficiente. Ad esempio, quando si lavora con API che richiedono solo credenziali di base o quando si utilizzano applicazioni legacy che non supportano altri metodi di autenticazione.

## Come Fare

```TypeScript
import axios from 'axios';

const username = 'myusername';
const password = 'mypassword';

axios.get('https://api.example.com/data', {
    auth: {
        username: username,
        password: password
    }
})
.then(response => {
    console.log(response.data);
})
.catch(error => {
    console.log(error);
})
```

Output:

```
{
   "id": 12345,
   "name": "John Doe"
}
```

## Deep Dive

Quando si invia una richiesta HTTP con autenticazione di base, è importante comprendere il processo di codifica delle credenziali. La specifica di autenticazione di base richiede che il nome utente e la password siano combinati in un'unica stringa separata da ":" e quindi codificati in base64. Questa stringa verrà poi inserita nell'header "Authorization" della richiesta HTTP.

## Vedi Anche

- [Specifiche di autenticazione di base HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Documentazione di Axios](https://github.com/axios/axios)