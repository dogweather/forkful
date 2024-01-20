---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing von HTML mittels JavaScript 

## Was & Warum?

HTML zu parsen bedeutet, den Code zu analysieren und zu interpretieren. Programmierer tun dies, um z.B. Inhalte aus einer Webseite zu extrahieren oder Web Scraping durchzuführen.

## So geht's:

Hier unser Beispiel:

``` Javascript
let parser, dom;
parser = new DOMParser();
dom = parser.parseFromString(' <!DOCTYPE html><p>JS ist toll!</p>', 'text/html');
console.log(dom.body.textContent);
```

Die Ausgabe dieses Programms ist:

``` Javascript
"JS ist toll!"
```

In diesem Code erzeugen wir mithilfe der `DOMParser()`-Methode einen neuen DOM-Parser. Anschließend nutzen wir `parseFromString()`, um HTML-Code zu einer DOM-Struktur zu konvertieren. Abschließend geben wir den Textinhalt des HTML-Dokuments aus.

## Vertiefung

Das Parsen von HTML hat eine lange Geschichte und war schon immer zentraler Bestandteil der Webentwicklung. Alternative Methoden sind etwa die Verwendung von Regex (nicht empfohlen) oder spezialisierten Libraries wie Cheerio oder JSDOM.

Während unserer Implementierung genutzte Methode, die DOMParser API, ist eine Web-API, die von den meisten modernen Browsern unterstützt wird. Es ermöglicht das Parsen von XML oder HTML zu einem Document-Objekt.

## Siehe auch

- [MDN Web Docs: DOMParser](https://developer.mozilla.org/de/docs/Web/API/DOMParser)
- [Web Scraping mit JavaScript und Node.js](https://www.freecodecamp.org/news/the-ultimate-guide-to-web-scraping-with-node-js-daa2027dcd3/)
- [Cheerio Fast, flexible & lean implementation of core jQuery](https://cheerio.js.org)
- [JSDOM](https://github.com/jsdom/jsdom)