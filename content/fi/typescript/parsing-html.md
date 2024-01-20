---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML-parsinta tarkoittaa HTML-dokumentin rakenteen analysoimista ja muuntamista ohjelmalliseen muotoon. Ohjelmoijat tekevät tämän, jotta voidaan helposti lukea, muuttaa ja manipuloida verkkosivun sisältöjä.

## Näin se tehdään:

Käytämme "jsdom" kirjastoa HTML-parsintaan TypeScriptissa. Tässä on esimerkkikoodi:

```TypeScript
import { JSDOM } from 'jsdom';

const htmlContent = `<body><h1>Terve Suomi!</h1></body>`;

const dom = new JSDOM(htmlContent);
```

Nyt voimme käyttää `dom` objektia HTML-elementtien käsittelyyn.

```TypeScript
let h1 = dom.window.document.querySelector('h1');
console.log(h1.textContent); // Tulostaa: "Terve Suomi!"
```

## Syvempi tarkastelu

HTML-parsinnalla on pitkä historia web-ohjelmoinnissa, kun ohjelmoijat alkoivat tarvita työkaluja HTML-dokumenttien käsittelyyn. On olemassa useita metodeja HTML-parsintaan, kuten SAX (Simple API for XML) ja DOM (Document Object Model).

"jsdom" on yksi monista kirjastoista, jolla voit tehdä HTML-parsintaa. Se luo "oikean" DOM-puun JavaScript-ympäristöissä, jolloin voit tehdä yhtä tehokkaasti HTML-parsintaa palvelinpäässä kuin selaimessa. 

Vaihtoehtoisesti, voit käyttää "cheerio" nimistä kirjastoa, joka on erittäin suorituskykyinen ja jQuery-tyylinen kirjasto HTML-parsintaan.

## Katso myös

Lisätietoja aiheesta voit löytää seuraavista lähteistä:

- "jsdom" npm paketti: https://www.npmjs.com/package/jsdom
- "cheerio" npm paketti: https://www.npmjs.com/package/cheerio
- MDN Web Docs, HTML-parsinnan perusteet: https://developer.mozilla.org/en-US/docs/Web/HTML/Parser