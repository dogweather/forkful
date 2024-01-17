---
title:                "Nedlasting av en nettside"
html_title:           "TypeScript: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Nedlasting av en nettside betyr å hente HTML-, CSS- og JavaScript-koden som utgjør nettsiden og vise den i nettleseren din. Programmerere gjør dette for å analysere nettsider, endre informasjon eller lage web scraping-verktøy som kan samle informasjon fra flere nettsteder.

## Hvordan: 
```TypeScript
import axios from 'axios';

// Hente en nettside
axios.get('https://www.eksempelnettsted.com/')
  .then(response => console.log(response.data))
  .catch(error => console.log(error));

// Endre brukeragenten
axios.get('https://www.eksempelnettsted.com/', {
  headers: {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36'}
})
  .then(response => console.log(response.data))
  .catch(error => console.log(error));
```

## Dypdykk:
Nedlasting av nettsider har blitt vanlig med den økende bruken av automatiserte og datadrevne løsninger på nettet. Det finnes også andre metoder for å hente nettsider, for eksempel gjennom nettleser-automatisering med verktøy som Puppeteer. Implementeringen av nedlasting av nettsider i TypeScript er enkel takket være eksterne biblioteker som axios og node-fetch.

## Se også:
- [Axios documentation](https://github.com/axios/axios)
- [Node-fetch documentation](https://www.npmjs.com/package/node-fetch)