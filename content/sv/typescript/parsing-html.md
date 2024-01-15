---
title:                "Att analysera html"
html_title:           "TypeScript: Att analysera html"
simple_title:         "Att analysera html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Det är vanligt att behöva hämta innehållet från en webbsida och använda det i en applikation eller skal. Att analysera HTML-koden och extrahera specifikt innehåll kan spara massor av tid och arbete för utvecklare.

## Hur man gör
### Installera och Importera
För att kunna analysera HTML-koden behöver vi använda ett TypeScript-paket som heter `cheerio`. Installera det genom att köra `npm install cheerio` i terminalen. Sedan kan vi importera det i vår kod genom att lägga till `import * as cheerio from 'cheerio';` i början av vår fil.

### Hämta HTML från en webbsida
Innan vi kan börja analysera HTML-koden behöver vi först hämta den från en URL. För detta använder vi `fetch()`-funktionen tillsammans med JavaScripts `Promise` för att få tillbaka data från ett GET-anrop. Vi kommer sedan konvertera svaret till text och skicka det som en parameter till `cheerio`-funktionen.

```TypeScript
const html = await fetch('https://mypage.com').then(res => res.text());
const $ = cheerio.load(html);
```

### Hitta och extrahera innehåll
Nu kan vi använda `cheerio` för att leta efter specifikt innehåll i vår HTML-kod. Det gör vi genom att använda CSS-selektorer tillsammans med `$(...)`-funktionen för att få tillbaka en array med alla element som matchar vårt selektor. Vi kan sedan använda `text()`- eller `attr()`-funktionerna för att få ut själva innehållet eller attributen från elementet.

```TypeScript
const title = $('h1').text(); // Hämtar innehållet i det första h1-elementet på sidan
const links = $('a'); // Returnerar en array med alla länkar på sidan
const imageSrc = $('img').attr('src'); // Hämtar URL:en till bilden i det första img-elementet på sidan
```

## Djupdykning
För att kunna använda CSS-selektorer behöver `cheerio` först konvertera HTML-koden till DOM-noder. Detta görs med hjälp av `htmlparser2`-paketet. Detta är också varför vi skickar in svaret från `fetch()`-anropet som en text-sträng till `cheerio`-funktionen.

Det finns också många andra funktioner som `cheerio` erbjuder, till exempel möjligheten att ändra på HTML-koden och sedan få tillbaka den ändrade koden med `html()`-funktionen.

## Se även
- [Officiell dokumentation för cheerio](https://github.com/cheeriojs/cheerio)
- [Fler exempel på hur man kan använda cheerio](https://www.digitalocean.com/community/tutorials/how-to-use-node-js-request-and-cheerio-to-set-up-simple-web-scraping)