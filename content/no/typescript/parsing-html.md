---
title:                "Analysering av HTML"
html_title:           "TypeScript: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Parsing av HTML er en prosess der programvare blir brukt til å analysere og tolke koden til et HTML-dokument. Dette gjøres vanligvis for å få tilgang til og manipulere innholdet i et dokument på en strukturert måte. Programmører bruker parsing for å automatisere behandlingen av store mengder data eller for å utvikle verktøy som forenkler nettutvikling.

## Slik gjør du det:
```TypeScript
// Eksempel på kode for å parse HTML ved hjelp av et tredjepartsbibliotek som kalles "cheerio"

import * as cheerio from 'cheerio';

// Hente og lagre HTML-koden fra et nettsted
const html = await fetch('https://www.nettside.com');

// Bruke cheerio til å laste HTML-koden og gjøre den tilgjengelig for å kunne jobbe med
const $ = cheerio.load(html);

// Eksempel på en parsing-forespørsel for å hente tittelen på en nettside
const title = $('h1').text();
console.log(title);
// Resultat: Utskrift av tittel fra nettsiden vår "Min nettside"
```

## Dykk dypere:
Parsing av HTML har blitt brukt siden de tidlige dagene av Internett og er fortsatt en vanlig metode for å behandle webinnhold. I tillegg til cheerio, finnes det også andre tredjepartsbiblioteker og innebygde funksjoner i TypeScript som kan brukes til parsing, som for eksempel "DOMParser" - et innebygd API for å analysere XML og HTML-dokumenter.

## Se også:
https://cheerio.js.org/ - Offisiell nettside for cheerio biblioteket
https://developer.mozilla.org/en-US/docs/Web/API/DOMParser - Mer informasjon om DOMParser APIet i JavaScript