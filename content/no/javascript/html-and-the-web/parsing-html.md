---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:44.896374-07:00
description: "\xC5 parse HTML betyr \xE5 trekke ut data fra HTML-dokumenter. Programmerere\
  \ gj\xF8r dette for \xE5 samhandle med eller manipulere webinnhold, automatisere\u2026"
lastmod: '2024-03-13T22:44:41.181773-06:00'
model: gpt-4-0125-preview
summary: "\xC5 parse HTML betyr \xE5 trekke ut data fra HTML-dokumenter."
title: Analysering av HTML
weight: 43
---

## Hvordan:
La oss parse HTML ved hjelp av `DOMParser`-APIen i JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hei, verden!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Utdata: Hei, verden!
```

Nå, la oss ta tak i noe mer spesifikt, som et element med en klasse:

```Javascript
const htmlString = `<div><p class="greeting">Hei, igjen!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Utdata: Hei, igjen!
```

## Dypdykk
Å parse HTML er like gammelt som weben selv. I starten var det en nettlesergreie – nettlesere parslet HTML for å vise nettsider. Over tid ønsket programmerere å tappe inn i denne prosessen, noe som førte til APIer som `DOMParser`.

Alternativer? Absolutt. Vi har biblioteker som `jQuery` og verktøy som `BeautifulSoup` for Python. Men JavaScripts innebygde `DOMParser` er rask og innebygd, ingen behov for ekstra biblioteker.

Når det gjelder implementering, når du parser HTML med `DOMParser`, oppretter det et `Document`-objekt. Tenk på det som en hierarkisk modell av HTML-en din. Når du har den, kan du navigere og manipulere den akkurat som du ville gjort med en normal nettsides DOM.

Men her er tingen – parsing kan snuble på feilformet HTML. Nettlesere er tilgivende, men `DOMParser` er kanskje ikke det. Derfor, for komplekse oppgaver eller rotete HTML, kan tredjepartsbiblioteker gjøre en bedre oppryddingsjobb.

## Se også
- MDN Web Docs om `DOMParser`-APIen: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery sin parsing-kapasiteter: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, en rask, fleksibel og slank implementering av kjernnen i jQuery for serveren: [Cheerio.js](https://cheerio.js.org/)
- For ikke-JS parsing: Pythons BeautifulSoup-bibliotek: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
