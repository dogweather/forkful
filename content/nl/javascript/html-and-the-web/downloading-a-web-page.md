---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:10.107168-07:00
description: 'Hoe te: Hier is een snelle manier om een pagina te downloaden met Node.js
  en `node-fetch`.'
lastmod: '2024-03-13T22:44:51.200091-06:00'
model: gpt-4-0125-preview
summary: Hier is een snelle manier om een pagina te downloaden met Node.js en `node-fetch`.
title: Een webpagina downloaden
weight: 42
---

## Hoe te:
Hier is een snelle manier om een pagina te downloaden met Node.js en `node-fetch`:

```Javascript
const fetch = require('node-fetch'); // je moet dit misschien eerst installeren!

async function downloadPage(url) {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Geeft de HTML-bron van de pagina weer
    } catch (error) {
        console.error(error);
    }
}

downloadPage('https://example.com');
```

Voorbeelduitvoer:

```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</html>
```

## Diepere Duik
Historisch gezien werd het downloaden van een webpagina gedaan met XMLHTTPRequest in de browser of `http` module in Node.js. Echter, na ES6, werd de `fetch` API de moderne standaard vanwege de eenvoudigere syntaxis en promise-gebaseerde aard.

Alternatieven zijn onder meer `axios`, een populair npm-pakket, dat verzoeken afhandelt met iets meer functionaliteit dan de native fetch. Voor complexe gebruiksscenario's zou je `puppeteer` kunnen gebruiken om de pagina daadwerkelijk te renderen in een headless browser, nuttig voor het omgaan met JavaScript-gerenderde inhoud.

Bij het implementeren van paginadownloads, let op aspecten zoals het respecteren van `robots.txt`, het afhandelen van `User-Agent` om blokkering te voorkomen, en het zorgvuldig managen van asynchrone afhandeling om potentiële valkuilen te vermijden zoals serveroverbelasting of racecondities.

## Zie Ook
- MDN Web Docs over de `fetch` API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Axios GitHub-pagina: https://github.com/axios/axios
- Puppeteer GitHub-pagina: https://github.com/puppeteer/puppeteer
- Een artikel over de beste praktijken bij web scraping: https://www.scrapingbee.com/blog/web-scraping-best-practices/
