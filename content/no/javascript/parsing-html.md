---
title:                "Analyse av HTML"
date:                  2024-01-20T15:32:19.547773-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Parsing HTML betyr å tolke og omforme HTML-koden slik at man kan manipulere den med programmering. Vi gjør det for å kunne hente ut, endre, eller interagere med innholdet på en nettside.

## How to: (Hvordan:)

```Javascript
// Eksempel: Hente ut innhold fra et HTML-element

const parser = new DOMParser();
const htmlString = '<div id="greeting">Hei, Verden!</div>';
const doc = parser.parseFromString(htmlString, 'text/html');
const content = doc.getElementById('greeting').textContent;

console.log(content);  // Output: "Hei, Verden!"
```

```Javascript
// Eksempel: Endre inneholdet i et HTML-element

const newHtmlString = '<div id="farewell">Ha det bra, Verden!</div>';
const newDoc = parser.parseFromString(newHtmlString, 'text/html');
doc.body.replaceChild(newDoc.getElementById('farewell'), doc.getElementById('greeting'));

console.log(doc.body.innerHTML);  // Output: "<div id="farewell">Ha det bra, Verden!</div>"
```

## Deep Dive (Dypdykk)

Parsing HTML er en gammel praksis, forenklet i moderne tid med DOMParser API-et, introdusert med HTML5. Tidligere brukte utviklere XMLHttpRequest for å hente og parse HTML, men det var mer omstendelig og mindre effektivt. 

Alternativer for parsing inkluderer biblioteker som jQuery, som forenkler seleksjonsprosessen, eller server-side verktøy som Beautiful Soup for Python, som ikke er avhengig av en nettleser. 

DOMParser parser HTML strengen til et ‘Document’ objekt, som man deretter kan manipulere som en vanlig DOM. Dette gjør det mulig å behandle strengebasert HTML som om den var en del av den originale nettsiden.

## See Also (Se Også)

- [MDN Web Docs: DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [JavaScript.info: Modifying the document](https://javascript.info/modifying-document)
- [W3C: HTML5 Parsing spec](https://www.w3.org/TR/html5/syntax.html#parsing)
