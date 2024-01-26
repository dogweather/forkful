---
title:                "HTML parsen"
date:                  2024-01-20T15:32:30.731816-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parsing ist der Prozess, bei dem man HTML-Code liest und dessen Struktur und Inhalt versteht. Programmierer machen das, um Daten aus Webseiten zu extrahieren, Inhalte zu manipulieren oder die Struktur für eigene Zwecke zu nutzen.

## How to:

Beispiel: HTML mit JavaScript parsen und Elemente auswählen:

```javascript
const parser = new DOMParser();
const htmlString = `
  <div>
    <p id="first">Hallo, Welt!</p>
    <p id="second">Parsing ist spaßig.</p>
  </div>
`;
const doc = parser.parseFromString(htmlString, "text/html");
const firstParagraph = doc.getElementById("first").textContent;
console.log(firstParagraph); // Gibt aus: "Hallo, Welt!"
```

## Deep Dive

Historisch gesehen war das Parsen von HTML oft kompliziert und fehleranfällig, besonders wegen schlecht strukturierten Codes und Browser-Inkonsistenzen. Alternativen zu `DOMParser` wie `jQuery.parseHTML()` haben die Aufgabe vereinfacht. Allerdings liefern moderne Web-APIs wie `DOMParser` eine saubere und standards-konforme Methode, die mit guten Performance-Ergebnissen überzeugt. Beim Implementieren ist darauf zu achten, dass der HTML-Code, den man parst, vertrauenswürdig ist, um Cross-Site-Scripting (XSS) Angriffe zu vermeiden.

## See Also

- MDN-Dokumentation zu `DOMParser`: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- XSS-Angriffe und wie man sie verhindert: https://owasp.org/www-community/attacks/xss/
- Eine Einführung in Web-Scraping mit JavaScript: https://javascript.info/parsing-html
