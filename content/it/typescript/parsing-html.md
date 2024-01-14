---
title:                "TypeScript: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Il parsing di HTML è una parte fondamentale della programmazione front-end. Questa operazione ci permette di estrarre informazioni strutturate dal codice HTML di una pagina web, rendendo possibile la manipolazione e la visualizzazione dei dati. Senza il parsing dell'HTML, sarebbe molto più difficile e dispendioso lavorare con il contenuto di una pagina web.

## Come Fare

Per eseguire il parsing di HTML in TypeScript, possiamo utilizzare la libreria esterna `cheerio`. Questa libreria ci permette di selezionare elementi del DOM usando una sintassi simile a jQuery, rendendo il processo molto più semplice e intuitivo. Vediamo un esempio pratico:

```TypeScript
// Importiamo la libreria
import * as cheerio from 'cheerio';

// Definiamo il codice HTML da analizzare
const html = `
  <html>
    <body>
      <h1> Il mio primo post </h1>
      <p> Benvenuti nel mio blog! </p>
    </body>
  </html>
`

// Utilizziamo la funzione load di cheerio per caricare il codice HTML
const $ = cheerio.load(html);

// Selezioniamo l'elemento <h1> e ne stampiamo il testo
console.log($('h1').text());
// Output: Il mio primo post
```

Come vediamo nell'esempio, attraverso l'utilizzo di `cheerio` siamo in grado di selezionare e manipolare facilmente gli elementi del DOM. Possiamo anche utilizzarla per estrarre dati da pagine web esterne, come ad esempio per realizzare degli script di web scraping.

## Approfondimento

Il processo di parsing dell'HTML non è sempre semplice come nell'esempio mostrato. Spesso ci troviamo ad affrontare pagine web complesse, con strutture diverse e elementi nidificati. In questi casi, è importante saper utilizzare le funzioni avanzate di `cheerio`, come ad esempio gli operatori di ricerca e i filtri. Imparare ad utilizzare questi strumenti ci permette di affrontare qualsiasi situazione e di estrarre i dati desiderati in modo efficiente.

## Vedi Anche

- [Documentazione di cheerio](https://cheerio.js.org/)
- [Tutorial su come utilizzare cheerio con TypeScript](https://dev.to/sabersight/how-to-web-scrape-with-cheerio-in-node-js-with-typescript-12ak)
- [Tutorial su come fare web scraping con TypeScript e cheerio](https://www.codementor.io/@johnaguiar/how-to-web-scrape-with-nodejs-and-cheerio-c9h5m0efs)