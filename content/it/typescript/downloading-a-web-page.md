---
title:                "Scaricare una pagina web"
html_title:           "TypeScript: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che vuole creare una applicazione web, è molto probabile che tu abbia bisogno di scaricare una pagina web per visualizzare il suo contenuto in un formato strutturato. Questo è dove TypeScript può aiutarti, facilitando il download di una pagina web e l'analisi dei suoi dati.

## Come Fare

```TypeScript
// importa il modulo http di Node.js
import * as http from 'http';

// definisci l'URL della pagina web che vuoi scaricare
const url = 'https://www.example.com';

// usa il metodo 'get' del modulo http per fare una richiesta GET all'URL
http.get(url, (res) => {
  // definisci una variabile per immagazzinare i dati ricevuti dalla pagina web
  let data = '';

  // aggiungi ogni pezzo di dato alla variabile 'data'
  res.on('data', (chunk) => {
    data += chunk;
  });

  // alla fine della richiesta, stampa il contenuto della pagina
  res.on('end', () => {
    console.log(data);
  });
}).on('error', (err) => {
  // gestisci eventuali errori
  console.log(err.message);
});
```

Esempio di output: `<html><head>...</head><body>...</body></html>`

## Approfondimento

Per scaricare una pagina web in TypeScript, viene utilizzato il modulo 'http' di Node.js. Ciò significa che è possibile utilizzare anche altri moduli Node.js per ulteriori funzionalità, come ad esempio 'fs' per la gestione dei file o 'cheerio' per l'analisi del DOM. Inoltre, con TypeScript, è possibile sfruttare le tipizzazioni dei dati per una migliore gestione degli errori durante il processo di download della pagina.

## Vedi Anche

- [Documentazione ufficiale di TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Modulo HTTP di Node.js](https://nodejs.org/api/http.html)
- [Modulo FS di Node.js](https://nodejs.org/api/fs.html)
- [Cheerio: un'utility per il parsing di HTML in Node.js](https://cheerio.js.org/)