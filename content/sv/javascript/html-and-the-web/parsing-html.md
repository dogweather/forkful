---
title:                "Analysera HTML"
aliases:
- /sv/javascript/parsing-html.md
date:                  2024-01-28T03:01:14.207115-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-html.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML innebär att extrahera data från HTML-dokument. Programmerare gör detta för att interagera med eller manipulera webbinnehåll, automatisera dataextraktion eller för webbskrapningsändamål.

## Hur man gör:
Låt oss tolka HTML med hjälp av `DOMParser`-API i JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Utmatning: Hello, world!
```

Nu, låt oss ta något mer specifikt, som ett element med en klass:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Utmatning: Hello, again!
```

## Djupdykning
Att tolka HTML är lika gammalt som webben. Inledningsvis var det en webbläsargrej - webbläsare tolkade HTML för att visa webbsidor. Över tid ville programmerare delta i denna process, vilket ledde till API:er som `DOMParser`.

Alternativ? Visst. Vi har bibliotek som `jQuery` och verktyg som `BeautifulSoup` för Python. Men JavaScripts inbyggda `DOMParser` är snabb och inbyggd, inget behov av extra bibliotek.

När det gäller implementering, när du tolkar HTML med `DOMParser`, skapas ett `Document`-objekt. Tänk på det som en hierarkisk modell av ditt HTML. När du har den kan du navigera och manipulera den precis som du skulle med en vanlig webbsidas DOM.

Här är grejen—tolkning kan snubbla på dåligt formaterad HTML. Webbläsare är förlåtande, men `DOMParser` kanske inte är det. Därför kan tredjepartsbibliotek göra ett bättre jobb med att städa upp i komplexa uppgifter eller rörigt HTML.

## Se även
- MDN Web Docs om `DOMParser`-API: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery:s tolkningskapaciteter: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, en snabb, flexibel och smal implementering av kärnjQuery för servern: [Cheerio.js](https://cheerio.js.org/)
- För icke-JS-tolkning: Pythons BeautifulSoup-bibliotek: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
