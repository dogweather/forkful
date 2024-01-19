---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# Analysering av HTML med Javascript

## Hva & Hvorfor?

Å analysere HTML, eller parsing, handler om å oversette en HTML-streng til et forståelig format for datamaskinen. Vi gjør dette for å ekstrahere informasjon og manipulere data på webområdet.

## Hvordan:

Her er et eksempel på hvordan parsing fungerer i praksis ved bruk av Det Innebygde DOM-parserobjektet i Javascript:

```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString('<h1>Hallo verden</h1>', 'text/html');
console.log(doc.body.firstChild.textContent); // "Hallo verden"
```

Koden ovenfor viser hvordan vi kan omdanne en HTML-streng til et element i Document Object Model (DOM), og deretter ekstrahere innholdet fra dette elementet.

## Dyp Dykk:

Historisk sett har HTML-parsers vært fundamentale for nettleseres evne til å vise nettsteder. Nå kan vi bruke parse-teknologi direkte i våre egne Javascript-applikasjoner for å manipulere og bruke HTML-data.

Alternativer til DOMParser inkluderer jQuery's `$.parseHTML()` og Node.js bibliotek som `cheerio`. Valget mellom disse avhenger av dine spesifikke behov og omgivelser.

Når det gjelder implementasjonsdetaljer, er det viktig å merke seg at parsing er en ressursintensiv oppgave, spesielt for større HTML-dokumenter, så sørg alltid for at du har effektiv kode.

## Se Også:

1. [MDN: DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
2. [jQuery: $.parseHTML()](https://api.jquery.com/jQuery.parseHTML/)
3. [Node.js: Cheerio](https://cheerio.js.org/)