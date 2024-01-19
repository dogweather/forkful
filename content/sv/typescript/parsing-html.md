---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML-tolkning handlar om att läsa HTML-kod och omvandla den till ett formellt format, vilket ofta är ett objektträd. Programmerare gör det för att extrahera, analysera och manipulera webbinnehåll, och till och med skrapa webbplatser.

## Hur man gör:
Följande är ett exempel på hur man tolkar HTML med Node.js och `jsdom` biblioteket i TypeScript:

```TypeScript
import { JSDOM } from 'jsdom';

async function parseHTML(html: string) {
   const dom = new JSDOM(html);
   return dom;
}

const html = "<div>Hej Världen!</div>";
parseHTML(html).then(dom => {
  console.log(dom.window.document.querySelector("div").textContent);
});
```

När du kör denna kod, borde output vara:

```
Hej Världen!
```

## Djupdykning
Historiskt sett har det funnits flera metoder för att tolka HTML, inklusive men långt ifrån begränsat till standard DOM-API: er och bibliotek som ``beautifulsoup`` (Python) och ``jquery`` (JavaScript). 

Ett alternativ till att använda ``jsdom`` är att använda cheerio-biblioteket, vilket kan ge bättre prestanda för server-side rendering men saknar förmågan att hantera JavaScript inuti HTML.

När det gäller implementation, konverterar `jsdom` hela HTML-strängen till ett DOM-träd i minnet. Detta kan leda till stora minnesfotavtryck för stora HTML-dokument, så se upp för det.

## Se även
- JSDOM GitHub repo: [https://github.com/jsdom/jsdom](https://github.com/jsdom/jsdom)
- Cheerio GitHub repo: [https://github.com/cheeriojs/cheerio](https://github.com/cheeriojs/cheerio)
- HTML-parsing med Node.js: [https://nodejs.org/api/all.html#all_console_dir_obj_options](https://nodejs.org/api/all.html#all_console_dir_obj_options)