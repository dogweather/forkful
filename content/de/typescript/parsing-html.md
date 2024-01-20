---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML ist der Prozess, in dem eine HTML-Datei analysiert und in eine für Computer verständlichere Struktur umgewandelt wird. Dies ist für Programmierer nützlich, um Daten zu extrahieren, Websites zu scrapen und Webinhalte zu manipulieren.

## Wie macht man das?

Einfaches HTML Parsen in TypeScript kann mit der Bibliothek "jsdom" erreicht werden. Installiere sie mit npm:

```TypeScript 
npm install jsdom
```

Erstelle danach eine rex.js Datei und füge den folgenden Code ein:

```TypeScript
import { JSDOM } from 'jsdom';

const html = `<body> Willkommen bei meinem Artikel </body>`;
const dom = new JSDOM(html);
console.log(dom.window.document.body.textContent); // "Willkommen bei meinem Artikel"
```

## Deep Dive 

HTML-Parsing hat eine lange Geschichte, die bis in die Anfänge des Internets zurückreicht. Zunächst bestanden Webseiten nur aus einfachem html. Heute gibt es viele Alternativen zum HTML-Parsing, wie zum Beispiel XML oder JSON.

JSDOM bietet uns eine einfache und effiziente Möglichkeit, HTML in TypeScript zu parsen. Es simuliert einen Webbrowser und ermöglicht es uns, DOM-Operationen auf der serverseite durchzuführen.

## Siehe auch

Weitere Informationen über HTML-Parsing und JSDOM findest du hier: 
- [JSdom Dokumentation](https://github.com/jsdom/jsdom)
- [XML vs HTML](https://www.w3schools.com/xml/xml_whatis.asp)