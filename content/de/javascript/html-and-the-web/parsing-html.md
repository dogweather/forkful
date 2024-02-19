---
aliases:
- /de/javascript/parsing-html/
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:44.405534-07:00
description: "HTML zu parsen bedeutet, Daten aus HTML-Dokumenten zu extrahieren. Programmierer\
  \ tun dies, um mit Webinhalten zu interagieren oder diese zu manipulieren,\u2026"
lastmod: 2024-02-18 23:09:05.276471
model: gpt-4-0125-preview
summary: "HTML zu parsen bedeutet, Daten aus HTML-Dokumenten zu extrahieren. Programmierer\
  \ tun dies, um mit Webinhalten zu interagieren oder diese zu manipulieren,\u2026"
title: HTML parsen
---

{{< edit_this_page >}}

## Was & Warum?
HTML zu parsen bedeutet, Daten aus HTML-Dokumenten zu extrahieren. Programmierer tun dies, um mit Webinhalten zu interagieren oder diese zu manipulieren, die Datenextraktion zu automatisieren oder für Web-Scraping-Zwecke.

## Wie:
Lassen Sie uns HTML mit der `DOMParser`-API in JavaScript parsen.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hallo, Welt!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Ausgabe: Hallo, Welt!
```

Nun, greifen wir etwas Spezifischeres ab, wie ein Element mit einer Klasse:

```Javascript
const htmlString = `<div><p class="greeting">Hallo, nochmal!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Ausgabe: Hallo, nochmal!
```

## Vertiefung
HTML zu parsen ist so alt wie das Web selbst. Anfangs war es eine Sache der Browser – Browser parsten HTML, um Webseiten anzuzeigen. Mit der Zeit wollten Programmierer in diesen Prozess eingreifen, was zu APIs wie `DOMParser` führte.

Alternativen? Sicher. Wir haben Bibliotheken wie `jQuery` und Werkzeuge wie `BeautifulSoup` für Python. Aber der native `DOMParser` von JavaScript ist schnell und eingebaut, ohne Bedarf für zusätzliche Bibliotheken.

Was die Umsetzung betrifft, wenn Sie HTML mit `DOMParser` parsen, wird ein `Document`-Objekt erstellt. Denken Sie daran als ein hierarchisches Modell Ihres HTML. Sobald Sie es haben, können Sie es navigieren und manipulieren, genau wie Sie es mit dem DOM einer normalen Webseite tun würden.

Hier ist die Sache – Parsen kann bei fehlerhaftem HTML ins Straucheln geraten. Browser sind nachsichtig, aber `DOMParser` ist es möglicherweise nicht. Deshalb könnten Bibliotheken von Drittanbietern bei komplexen Aufgaben oder unordentlichem HTML eine bessere Reinigungsarbeit leisten.

## Siehe auch
- MDN Web Docs zur `DOMParser`-API: [MDN DOMParser](https://developer.mozilla.org/de/docs/Web/API/DOMParser)
- jQuery’s Parsing-Fähigkeiten: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, eine schnelle, flexible und schlanke Implementierung des Kerns von jQuery für den Server: [Cheerio.js](https://cheerio.js.org/)
- Für das Nicht-JS-Parsing: Pythons BeautifulSoup-Bibliothek: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
