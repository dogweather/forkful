---
date: 2024-01-20 17:44:29.272431-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:38.292196-06:00'
model: gpt-4-1106-preview
summary: .
title: "H\xE4mta en webbsida"
weight: 42
---

## Hur man gör:
```Javascript
const https = require('https');
const fs = require('fs');

const downloadPage = (url, outputPath) => {
  https.get(url, response => {
    let data = '';
    response.on('data', chunk => { data += chunk; });
    response.on('end', () => {
      fs.writeFileSync(outputPath, data);
      console.log('Sidan nedladdad:', outputPath);
    });
  }).on('error', err => {
    console.error('Fel vid nedladdning:', err);
  });
};

downloadPage('https://example.com', 'example.html');
```
Förväntad output:
```
Sidan nedladdad: example.html
```

## Fördjupning
Historiskt sett har webbskrapning alltid varit ett vanligt sätt att extrahera data från webben, men det blir allt svårare med komplexa webbsidor som använder JavaScript och AJAX. Alternativ till `https.get` inkluderar `axios` eller `fetch` för moderna applikationer, medan `puppeteer` och `cheerio` är bra för JavaScript-driven innehåll och enklare parsing av HTML. Det är också viktigt att hantera användarrättigheter och att vara medveten om webbplatsernas användarvillkor vid nedladdning och skrapning av data.

## Se även
- MDN Web Docs om Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios GitHub repo: https://github.com/axios/axios
- Puppeteer GitHub repo: https://github.com/puppeteer/puppeteer
- Cheerio GitHub repo: https://github.com/cheeriojs/cheerio
