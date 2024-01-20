---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Scaricare una pagina web con Javascript

## Che cosa & Perché?

Scaricare una pagina web significa recuperare i dati di quella pagina dal server al tuo computer. Lo facciamo per visualizzare il contenuto della pagina, analizzarlo o conservarlo per utilizzi futuri.

## Come fare:

Il modulo "axios" è un metodo popolare per scaricare pagine web. Puoi installarlo con `npm install axios`.

```Javascript
const axios = require('axios');
const fs = require('fs');

axios.get('https://www.google.it')
  .then(response => {
    fs.writeFileSync('pagina.html', response.data);
    console.log('Pagina scaricata con successo!');
  })
  .catch(error => {
    console.error('Si è verificato un errore!', error);
  });
```

Ecco un esempio di output:

```Javascript
Pagina scaricata con successo!
```

## Approfondimento

Historicamente, si usavano le librerie come `http` o `https` built-in di Node.js per scaricare pagine web, ma queste librerie sono verbose e complesse da utilizzare.

Un'alternativa è usare `fetch`, che è uno standard moderno e più semplice rispetto a `axios`, ma richiede polifilling su Node.js.

In termini di implementazione, quando scarichi una pagina web, stai effettivamente inviando una richiesta HTTP GET al server che ospita quella pagina. Il server risponde con i dati della pagina, che il tuo codice può poi utilizzare.

## Vedi anche

- Documentazione axios: <https://github.com/axios/axios>
- Documentazione fetch: <https://developer.mozilla.org/it/docs/Web/API/Fetch_API>
- Documentazione HTTP di Node.js: <https://nodejs.org/api/http.html>