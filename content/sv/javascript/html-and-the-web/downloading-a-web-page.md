---
date: 2024-01-20 17:44:29.272431-07:00
description: "Att ladda ner en webbsida betyder att man h\xE4mtar dess HTML, CSS och\
  \ eventuellt JavaScript och annat inneh\xE5ll. Programmerare g\xF6r detta f\xF6\
  r att analysera\u2026"
lastmod: 2024-02-19 22:04:57.534812
model: gpt-4-1106-preview
summary: "Att ladda ner en webbsida betyder att man h\xE4mtar dess HTML, CSS och eventuellt\
  \ JavaScript och annat inneh\xE5ll. Programmerare g\xF6r detta f\xF6r att analysera\u2026"
title: "H\xE4mta en webbsida"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida betyder att man hämtar dess HTML, CSS och eventuellt JavaScript och annat innehåll. Programmerare gör detta för att analysera sidinnehåll, testa prestanda eller skrapa data.

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
