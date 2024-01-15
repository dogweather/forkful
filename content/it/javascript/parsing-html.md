---
title:                "Analisi delle intestazioni HTML"
html_title:           "Javascript: Analisi delle intestazioni HTML"
simple_title:         "Analisi delle intestazioni HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati provenienti da pagine web, potrebbe essere necessario estrarre informazioni specifiche da loro. La soluzione per fare ciò è di usare il parsing HTML, ovvero l'analisi e l'elaborazione del codice HTML di una pagina web.

## Come Fare

Per eseguire il parsing HTML in Javascript, puoi utilizzare la libreria "cheerio". Ecco un esempio di come potresti estrarre il titolo di una pagina web:

```Javascript
const cheerio = require('cheerio');
const request = require('request');

const url = 'http://example.com';

request(url, (error, response, html) => {
    if (!error && response.statusCode == 200) {
        const $ = cheerio.load(html);
        const title = $('title').text();
        console.log(title);
    }
});
```

Il risultato di questo codice dovrebbe essere il titolo della pagina "Example Domain". 

## Approfondimento

Il parsing HTML è fondamentale per lo sviluppo di applicazioni web e per l'estrazione di informazioni da pagine web. Il processo coinvolge l'analisi del documento HTML e la sua trasformazione in un formato facilmente manipolabile dai programmatori. La libreria "cheerio" offre un'API simile a JQuery che semplifica il processo di selezione e manipolazione degli elementi della pagina web.

## Vedi Anche

- [Documentazione di Cheerio](https://cheerio.js.org/)
- [Tutorial su Come Estrarre Dati da una Pagina Web Usando Javascript e Cheerio](https://www.digitalocean.com/community/tutorials/how-to-scrape-a-website-using-node-js-and-cheerio)