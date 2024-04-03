---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:01.587235-07:00
description: "HTML parseren betekent gegevens extraheren uit HTML-documenten. Programmeurs\
  \ doen dit om te interageren met of webinhoud te manipuleren, gegevensextractie\u2026"
lastmod: '2024-03-13T22:44:51.199123-06:00'
model: gpt-4-0125-preview
summary: HTML parseren betekent gegevens extraheren uit HTML-documenten.
title: HTML Parsen
weight: 43
---

## Hoe:
Laten we HTML parseren met de `DOMParser` API in JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hallo, wereld!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Uitvoer: Hallo, wereld!
```

Laten we nu iets specifiekers pakken, zoals een element met een klasse:

```Javascript
const htmlString = `<div><p class="greeting">Hallo, opnieuw!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Uitvoer: Hallo, opnieuw!
```

## Uitdieping
HTML parseren is zo oud als het web. Aanvankelijk was het een browser-ding—browsers parseerden HTML om webpagina's te tonen. Na verloop van tijd wilden programmeurs in dit proces tappen, wat leidde tot API's zoals `DOMParser`.

Alternatieven? Zeker. We hebben bibliotheken zoals `jQuery` en tools zoals `BeautifulSoup` voor Python. Maar JavaScript's native `DOMParser` is snel en ingebouwd, geen noodzaak voor extra bibliotheken.

Wat betreft de implementatie, wanneer je HTML parseert met `DOMParser`, creëert het een `Document` object. Beschouw het als een hiërarchisch model van je HTML. Eenmaal in bezit, kun je erdoor navigeren en manipuleren net alsof je met de DOM van een normale webpagina werkt.

Hier is het ding—parsing kan struikelen over slecht geformeerde HTML. Browsers zijn vergevingsgezind, maar `DOMParser` is dat misschien niet. Vandaar, voor complexe taken of rommelige HTML, kunnen bibliotheken van derden mogelijk beter opruimen.

## Zie Ook
- MDN Web Docs over de `DOMParser` API: [MDN DOMParser](https://developer.mozilla.org/nl/docs/Web/API/DOMParser)
- jQuery’s parseercapaciteiten: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, een snelle, flexibele & lean implementatie van de core jQuery voor de server: [Cheerio.js](https://cheerio.js.org/)
- Voor niet-JS parsing: Python’s BeautifulSoup bibliotheek: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
