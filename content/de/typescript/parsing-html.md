---
title:                "HTML parsen"
date:                  2024-01-20T15:34:11.911480-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
HTML-Parsing ist das Einlesen und Umwandeln von HTML-Dokumenten in eine strukturierte Form, die von Programmen genutzt werden kann. Programmierer machen das, um Inhalte zu extrahieren, zu manipulieren oder um HTML-Dokumente maschinell zu analysieren.

## How to (Wie geht das?)
```TypeScript
import { parse } from 'node-html-parser';

const html = '<html><body><p>Hello, World!</p></body></html>';
const root = parse(html);

// Zugriff auf den Inhalt des Paragraphen
const paragraphText = root.querySelector('p').innerText;
console.log(paragraphText); // Hello, World!

// Ausgabe der HTML-Struktur
console.log(root.toString()); // Gibt das Original-HTML zurück
```
Sample Output:
```
Hello, World!
<html><body><p>Hello, World!</p></body></html>
```

## Deep Dive (Tiefer eintauchen)
HTML-Parsing ist seit den Anfängen des Web ein Thema. Ursprünglich nutzten Browser-integrierte Parser, aber serverseitige Anforderungen führten zur Entwicklung von Tools wie BeautifulSoup für Python oder JSoup für Java.

In TypeScript bzw. JavaScript hat sich das NPM-Paket `node-html-parser` als praktikable Lösung etabliert. Es wandelt HTML in ein DOM-ähnliches Objekt um, wodurch der Zugriff auf Elemente und die Manipulation erleichtert wird.

Warum nicht einfach `RegExp`? Reguläre Ausdrücke sind nicht für verzweigte Strukturen wie HTML ausgelegt und können zu fehleranfälligem Code führen.

Alternativen? Der HTML Living Standard empfiehlt das DOM Parsing API für Browser, während in Node.js Cheerio oder JSDOM beliebt sind.

## See Also (Siehe auch)
- MDN Web Docs zum DOM Parsing API: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- node-html-parser auf NPM: https://www.npmjs.com/package/node-html-parser
- Cheerio NPM Paket: https://www.npmjs.com/package/cheerio
- JSDOM NPM Paket: https://www.npmjs.com/package/jsdom
