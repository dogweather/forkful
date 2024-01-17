---
title:                "Herunterladen einer Webseite"
html_title:           "TypeScript: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite ist der Vorgang des Abrufs von HTML, CSS, JavaScript und anderen Dateien von einem bestimmten URL. Programmierer nutzen diesen Prozess, um Inhalte von einer Webseite auf einer anderen Seite anzuzeigen oder um sie offline zu nutzen.

## So geht's:
```TypeScript
import fetch from 'node-fetch';

// URL der herunterzuladenen Seite
const url = "https://www.example.com";

fetch(url) 
    .then(response => response.text())
    .then(data => console.log(data))
    .catch(error => console.log(error));

```

## Tiefere Einblicke:
Das Herunterladen von Webseiten hat eine lange Geschichte, die bis in die Anfänge des Internets zurückreicht. Früher wurde dies hauptsächlich durch das Herunterladen von Dateien über FTP oder HTTP ermöglicht. Heutzutage gibt es viele alternative Methoden, wie zum Beispiel das Herunterladen von Inhalten über APIs oder das Verwenden von Web Crawlers. In TypeScript können wir die node-fetch Library verwenden, um die Inhalte von Webseiten herunterzuladen. Dabei müssen wir jedoch darauf achten, die Daten korrekt zu verarbeiten, da sie möglicherweise im falschen Format vorliegen oder Fehler enthalten können.

## Siehe auch:
- [node-fetch Dokumentation](https://www.npmjs.com/package/node-fetch)
- [Web Crawling Artikel](https://de.wikipedia.org/wiki/Web_Crawling)