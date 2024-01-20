---
title:                "Tolka HTML"
date:                  2024-01-20T15:34:13.723455-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Parsing HTML innebär att vi läser och tolkar HTML-kod för att förstå dess struktur och innehåll. Programmerare gör detta för att manipulera, extrahera eller interagera med webbinnehåll.

## How to: (Hur?)
Vi använder `DOMParser` för att parse:a HTML i TypeScript.

```typescript
const htmlString = `<p>Hej världen!</p>`;
const parser = new DOMParser();
const doc = parser.parseFromString(htmlString, 'text/html');

console.log(doc.body.firstChild?.textContent); // Output: "Hej världen!"
```

Ett mer komplext exempel, där vi får tag på element via klassnamn:

```typescript
const htmlString = `<div><p class="hälsning">Hej igen!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');

const greeting = doc.querySelector('.hälsning');
console.log(greeting?.textContent); // Output: "Hej igen!"
```

## Deep Dive (Djupdykning)
Förr i tiden extraherade vi data ur HTML med regular expressions, men det var knepigt och osäkert. DOMParser erbjuder en robust lösning som följer webbstandarder, vilket gör det enklare att arbeta med XML- eller HTML-dokument. 

JavaScript-bibliotek som Cheerio är alternativ som kör på serversidan och erbjuder jQuery-liknande syntax för att navigera DOM-trädet. Andra ramverk som JSDom låter dig simulera en webbläsarmiljö.

När du använder `DOMParser` bör du tänka på säkerhetsaspekter som innebär att inte manipulera DOM:n med farligt innehåll (t.ex., `XSS`-attacker).

## See Also (Se också)
- MDN Web Docs om DOMParser: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- Cheerio GitHub-sida: https://github.com/cheeriojs/cheerio
- JSDom GitHub-sida: https://github.com/jsdom/jsdom