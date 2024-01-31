---
title:                "Inviare una richiesta http con autenticazione di base"
date:                  2024-01-20T18:02:41.955121-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
L'autenticazione base HTTP serve per accedere a risorse protette via web usando username e password. I programmatori la usano per una semplice e diretta protezione delle comunicazioni fra client e server.

## How to:
Ecco un esempio rapido di come inviare una richiesta HTTP con autenticazione base in TypeScript usando `fetch`.

```typescript
import fetch from 'node-fetch';

const url = 'https://api.esempio.com/dati';
const username = 'utente';
const password = 'password';

const headers = new Headers({
  Authorization: 'Basic ' + Buffer.from(`${username}:${password}`).toString('base64')
});

async function fetchData() {
  try {
    const response = await fetch(url, { method: 'GET', headers: headers });

    if (!response.ok) {
      throw new Error(`Errore HTTP! status: ${response.status}`);
    }

    console.log('Risposta:', await response.json());
  } catch (error) {
    console.error('Errore durante il fetch:', error);
  }
}

fetchData();
```

Output di esempio:

```plaintext
Risposta: { ...dati della risposta... }
```

## Deep Dive
L'autenticazione base HTTP esiste da gli inizi del web. È uno dei metodi più semplici: l'header `Authorization` contiene le parole "Basic" seguite dall'encoding Base64 di `username:password`.

Esistono alternative più sicure come OAuth e JWT che non espongono direttamente le credenziali. L’uso dell’autenticazione base è sconsigliato su connessioni non criptate come HTTP, preferisci sempre HTTPS.

Dettagli di implementazione: in TypeScript usiamo `Buffer.from()` per convertire username e password in Base64. L'oggetto `Headers` è parte dell'API Fetch, che gestisce le nostre richieste asincrone.

## See Also
- Documentazione di Fetch API: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Guida a Node.js `Buffer`: [Node.js Docs](https://nodejs.org/api/buffer.html#buffer_class_buffer)
