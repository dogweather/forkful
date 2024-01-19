---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cosa e Perche?
Scaricare una pagina web significa prelevare i dati di essa dal server web e portarli sul proprio dispositivo. I programmatori fanno ciò per analizzare i dati, testare l'applicazione o eseguire il web scraping.

## Come fare:
Per scaricare una pagina web con TypeScript, possiamo utilizzare librerie come Axios e jsdom. Ecco un esempio:

```TypeScript
import axios from 'axios';
import {JSDOM} from 'jsdom';

async function scaricarePagina(url: string) {
  const risposta = await axios.get(url);
  const dom = new JSDOM(risposta.data);
  console.log(dom.window.document.querySelector('title').textContent);
}

scaricarePagina('https://example.com');
```
Risultato:

```Command Output
Example Domain
```

## Approfondimenti
(1) **Contesto storico**: Originariamente, le pagine web erano soltanto file HTML statici scaricati dai server web. Ora, con l'evoluzione degli script lato client, è necessario eseguire il codice JavaScript incluso nelle pagine web per ottenere i dati completi della pagina.

(2) **Alternative**: Oltre ad Axios e JSDOM, ci sono altre librerie come PhantomJS o Puppeteer che consentono un'interazione più complessa con la pagina web, inclusa l'esecuzione del JavaScript. 

(3) **Dettagli dell'implementazione**: Axios ottiene i dati della pagina web facendo una richiesta HTTP GET al server. Successivamente, questi dati vengono passati a JSDOM, che crea una rappresentazione DOM della pagina che possiamo manipolare con JavaScript.

## Vedere Anche
- [Axios](https://github.com/axios/axios) - Promising based HTTP client for Node.js and browser
- [jsdom](https://github.com/jsdom/jsdom) - A JavaScript implementation of the WhatWG DOM and HTML standards
- [Puppeteer](https://github.com/puppeteer/puppeteer) - Headless Chrome or Chromium browsers automation library
- [PhantomJS](http://phantomjs.org/) - Scriptable Headless Browser (progetto non più mantenuto attivamente)