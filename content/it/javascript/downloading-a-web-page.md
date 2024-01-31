---
title:                "Scaricare una pagina web"
date:                  2024-01-20T17:44:27.408580-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e perché?)
Scaricare una pagina web significa prelevare il suo contenuto HTML, CSS e JavaScript dal server a locale. Si fa per elaborare dati, fare test o archiviare informazioni.

## How to: (Come fare)
Usiamo `fetch()` per prendere il contenuto di una pagina web. Guarda qui:

```javascript
// Metodo asincrono per scaricare il contenuto di una pagina web
async function downloadWebPage(url) {
  try {
    const response = await fetch(url);
    const data = await response.text();
    console.log(data); // Qui abbiamo i dati della pagina
  } catch (error) {
    console.error('Errore nel download:', error);
  }
}

// Esempio di utilizzo
downloadWebPage('https://www.example.com');
```

Se lavori in Node.js, ti serve `node-fetch` o `axios`. Così:

```javascript
const fetch = require('node-fetch'); // Oppure importa axios

fetch('https://www.example.com')
  .then(response => response.text())
  .then(data => {
    console.log(data); // Ecco il tuo HTML!
  })
  .catch(error => {
    console.error('Errore:', error);
  });
```

## Deep Dive (Approfondimento)
La funzione `fetch()` è parte dell'API Fetch moderna, che a partire da HTML5 ha soppiantato `XMLHttpRequest`. `fetch()` supporta promesse e `async/await` per un codice più pulito e leggero.

Alternative come `axios` possono offrire vantaggi come l'intercettazione delle richieste, timeout automatici, e protezione contro attacchi CSRF. `node-fetch` è un polyfill di `fetch()` per Node.js.

Per scaricare file più grandi o avere più controllo, considera moduli come `request` (sebbene al 2023 sia deprecato) o librerie come `got`, che gestiscono stream e performance meglio.

Il download completo, compresi script e stili, può richiedere parsing del DOM e gestione delle risorse incrociate. Framework come Puppeteer o Playwright simulano l'intero browser per questo.

## See Also (Vedi anche)
- MDN Web Docs su Fetch API: https://developer.mozilla.org/it/docs/Web/API/Fetch_API
- GitHub Axios: https://github.com/axios/axios
- Node-fetch: https://www.npmjs.com/package/node-fetch
- Puppeteer: https://pptr.dev/
- Playwright: https://playwright.dev/
