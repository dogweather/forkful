---
date: 2024-01-20 18:02:03.108682-07:00
description: 'Come Fare: Ecco un esempio su come inviare una richiesta HTTP con autenticazione
  base in JavaScript usando `fetch`.'
lastmod: '2024-03-13T22:44:43.811852-06:00'
model: gpt-4-1106-preview
summary: Ecco un esempio su come inviare una richiesta HTTP con autenticazione base
  in JavaScript usando `fetch`.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## Come Fare:
Ecco un esempio su come inviare una richiesta HTTP con autenticazione base in JavaScript usando `fetch`.

```Javascript
const url = 'https://api.esempio.com/dati';
const username = 'utente';
const password = 'passwordSicura123';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ":" + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) {
      return response.json();
    }
    throw new Error('Qualcosa è andato storto...');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Errore:', error));
```

Se tutto va bene, vedrai i dati richiesti nel log della console.

## Approfondimento:
L'autenticazione base HTTP è un sistema vecchio quanto il web. Invia le credenziali in modo semplice, codificandole in Base64. Sfortunatamente, questo non è molto sicuro su connessioni non HTTPS poiché le credenziali possono essere intercettate facilmente. Tuttavia, resta un modo rapido per prototipare o per sistemi con minore esigenza di sicurezza.

Alternative più sicure includono OAuth e token di autenticazione. Per implementazioni critiche, è meglio evitare l'autenticazione base o assicurarsi che sia sempre usata su connessioni sicure (HTTPS).

## Vedi Anche:
- Documentazione di Mozilla MDN su `fetch`: https://developer.mozilla.org/it/docs/Web/API/Fetch_API/Using_Fetch
- Introduzione all'autenticazione HTTP su MDN: https://developer.mozilla.org/it/docs/Web/HTTP/Authentication
- Informazioni sulla codifica Base64 su MDN: https://developer.mozilla.org/it/docs/Glossary/Base64
