---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:49.630402-07:00
description: "Kuinka: J\xE4sennet\xE4\xE4n HTML `DOMParser`-API:n avulla JavaScriptiss\xE4\
  ."
lastmod: '2024-03-13T22:44:56.947743-06:00'
model: gpt-4-0125-preview
summary: "J\xE4sennet\xE4\xE4n HTML `DOMParser`-API:n avulla JavaScriptiss\xE4."
title: "HTML:n j\xE4sent\xE4minen"
weight: 43
---

## Kuinka:
Jäsennetään HTML `DOMParser`-API:n avulla JavaScriptissä.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Tuloste: Hello, world!
```

Nyt, otetaan kiinni jotain spesifimpää, kuten elementti luokalla:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Tuloste: Hello, again!
```

## Syväsukellus
HTML:n jäsennys on yhtä vanha kuin web. Aluksi se oli selainjuttu—selaimet jäsensivät HTML:n näyttääkseen web-sivuja. Ajan myötä ohjelmoijat tahtoivat päästä käsiksi tähän prosessiin, mikä johti APIeihin kuten `DOMParser`.

Vaihtoehtoja? Tietysti. Meillä on kirjastoja kuten `jQuery` ja työkaluja kuten `BeautifulSoup` Pythonille. Mutta JavaScriptin natiivi `DOMParser` on nopea ja sisäänrakennettu, ei tarvetta ylimääräisille kirjastoille.

Toteutuksen kannalta, kun jäsennät HTML:ää `DOMParser`in avulla, se luo `Document`-objektin. Ajattele sitä hierarkkisena mallina HTML:stäsi. Kun sinulla se on, voit navigoida ja manipuloida sitä juuri kuten normaalin web-sivun DOMin kanssa.

Tässä se juttu—jäsennys voi kompastua virheelliseen HTML:ään. Selaimet ovat anteeksiantavia, mutta `DOMParser` ei ehkä ole. Siksi, monimutkaisiin tehtäviin tai sekavaan HTML:ään, kolmannen osapuolen kirjastot saattavat tehdä parempaa siivousta.

## Katso Myös
- MDN Web Docs `DOMParser`-API:sta: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQueryn jäsennyskyvyt: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, nopea, joustava & kevyt toteutus core jQuerysta palvelimelle: [Cheerio.js](https://cheerio.js.org/)
- Ei-JS jäsennys: Pythonin BeautifulSoup-kirjasto: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
