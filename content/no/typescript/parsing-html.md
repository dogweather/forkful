---
title:                "TypeScript: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Parsing av HTML er en viktig del av moderne webutvikling. Det lar deg trekke ut spesifikke data fra nettsider og bruke dem til å bygge mer interaktive og tilpassede applikasjoner. I denne bloggposten vil vi utforske hvordan du kan parse HTML ved hjelp av TypeScript.

## Hvordan
```TypeScript
// Importer cheerio-biblioteket for å parse HTML
import * as cheerio from 'cheerio';

// Lag en "html" variabel som inneholder den nettsiden du ønsker å parse
const html = `<div class="person">
  <h2>John Doe</h2>
  <p>Age: 30</p>
  <p>Occupation: Developer</p>
</div>`;

// Bruk cheerio til å laste inn HTML-en i en variabel
const $ = cheerio.load(html);

// Bruk vanlige CSS-selektorer til å finne spesifikke elementer og hente ut dataen du trenger
const person = $('.person');
const name = person.find('h2').text();
const age = person.find('p').eq(0).text().replace('Age: ', '');
const occupation = person.find('p').eq(1).text().replace('Occupation: ', '');

// Skriv ut resultatet
console.log(`${name} er ${age} år gammel og jobber som ${occupation}.`);

// Output: John Doe er 30 år gammel og jobber som Developer.
```

I dette eksempelet brukte vi cheerio-biblioteket for å parse en enkel HTML-struktur og hente ut dataen vi trengte. Dette er bare et enkelt eksempel, og det finnes mange forskjellige måter å bruke TypeScript på for å parse HTML.

## Dypdykk
Når vi bruker TypeScript til å parse HTML, er det viktig å forstå hvordan selektorer fungerer. Cheerio bruker selektorer som ligner på CSS-selektorer for å finne spesifikke elementer på nettsiden. Ved å forstå hvordan disse selektorene fungerer, kan du enkelt hente ut dataen du trenger fra HTML-en.

Det er også viktig å forstå at cheerio ikke kjører JavaScript på nettsiden, det laster bare inn HTML-en og lar deg manipulere den. Dette betyr at det er begrensninger på hva du kan gjøre med cheerio, og det kan være nødvendig å bruke andre verktøy eller biblioteker for mer avansert parsing.

## Se også
- [Cheerio dokumentasjon (engelsk)](https://cheerio.js.org/)
- [En guide til å parse HTML med TypeScript (engelsk)](https://www.sitepoint.com/web-scraping-with-node-and-cheerio/)
- [TypeScript offisiell dokumentasjon (engelsk)](https://www.typescriptlang.org/)