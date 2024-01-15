---
title:                "Laste ned en nettside"
html_title:           "TypeScript: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du vil hente en nettside og bruke den i et program eller analysere dataene, kan nedlasting av en nettside være nyttig.

## Hvordan

```TypeScript
// Importer node-fetch biblioteket
import fetch from 'node-fetch';

// Lag en funksjon for å laste ned en nettside
const downloadWebpage = async (url: string) => {
  // Bruk fetch til å hente nettsiden
  const response = await fetch(url);
  // Konverter responsen til tekst
  const webpage = await response.text();
  // Skriv ut nettsiden
  console.log(webpage);
};

// Kjør funksjonen med ønsket URL
downloadWebpage('https://www.example.com');
```

Dette vil skrive ut HTML-koden for nettsiden som responsen fra nedlastingen.

## Dypdykk

Det er flere måter å laste ned en nettside på, avhengig av hva du ønsker å få fra den. Du kan for eksempel bruke biblioteker som Axios, Request eller Puppeteer. Du kan også bruke kodespråk som Python eller Java for å laste ned en nettside. Det er viktig å være forsiktig når du laster ned nettsider, da noen nettsteder blokkerer uautoriserte nedlastinger.

## Se også

- Node.js: https://nodejs.org/en/
- node-fetch bibliotek: https://www.npmjs.com/package/node-fetch
- Axios bibliotek: https://www.npmjs.com/package/axios
- Request bibliotek: https://www.npmjs.com/package/request
- Puppeteer bibliotek: https://www.npmjs.com/package/puppeteer