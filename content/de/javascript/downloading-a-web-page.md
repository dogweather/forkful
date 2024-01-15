---
title:                "Das Herunterladen einer Webseite"
html_title:           "Javascript: Das Herunterladen einer Webseite"
simple_title:         "Das Herunterladen einer Webseite"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum jemand eine Webseite herunterladen möchte. Vielleicht möchtest du eine lokale Kopie einer Webseite machen, um sie offline anzusehen oder zu sichern. Oder du möchtest die Webseitenstruktur analysieren oder bestimmte Informationen extrahieren. Egal aus welchem Grund, das Herunterladen einer Webseite kann mit JavaScript einfach und effektiv durchgeführt werden.

## How To

Die Kernfunktion für das Herunterladen einer Webseite mit JavaScript ist `XMLHttpRequest`. Diese ermöglicht es uns, eine Anfrage an eine URL zu senden und die Antwort als HTML-Text zu erhalten. Hier ist ein Beispielcode:

```Javascript
// Erstelle ein neues XMLHttpRequest-Objekt
let xhr = new XMLHttpRequest();
// Öffne eine Anfrage an eine URL
xhr.open('GET', 'https://www.beispielwebseite.de');
// Erhalte die Antwort als HTML
xhr.responseType = 'document';
// Rufe die Anfrage auf
xhr.send();
// Warte auf die Antwort und führe eine Funktion aus
xhr.onload = function() {
  // Speichere die Antwort in einer Variablen
  let response = xhr.response;
  // Greife auf das HTML-Dokument zu
  let html = response.documentElement;
  // Extrahiere den gesamten HTML-Text der Seite
  let htmlText = html.outerHTML;
  // Gib den HTML-Text in der Konsole aus
  console.log(htmlText);
}
```

In diesem Beispiel verwenden wir `XMLHttpRequest`, um eine GET-Anfrage an die URL `https://www.beispielwebseite.de` zu senden. Wir erhalten die Antwort als HTML-Text, indem wir die Eigenschaft `responseType` auf `document` setzen. Dann rufen wir die Anfrage auf und warten auf die Antwort. Sobald die Antwort erhalten wurde, greifen wir mithilfe der Eigenschaft `response` auf das HTML-Dokument zu und speichern es in einer Variablen. Von dort aus können wir den gesamten HTML-Text extrahieren und ihn in der Konsole ausgeben.

## Deep Dive

Es gibt noch weitere Möglichkeiten, wie man mithilfe von JavaScript Webseiten herunterladen kann. Eine Alternative zur `XMLHttpRequest` ist die Verwendung von `fetch`, die es uns ermöglicht, asynchrone Netzwerkanfragen durchzuführen. Die Funktionsweise ist ähnlich wie im vorherigen Beispiel, nur dass wir hier die Antwort als `text()` oder `json()` konvertieren müssen, um den HTML-Text zu erhalten.

Außerdem gibt es Bibliotheken wie Puppeteer und Cheerio, die uns noch mehr Kontrolle über das Herunterladen und Verarbeiten von Webseiten geben. Mit Puppeteer können wir sogar automatisiert durch Webseiten navigieren und Screenshots erstellen.

Es ist jedoch wichtig zu beachten, dass das Herunterladen einer Webseite mit JavaScript einige Einschränkungen hat. Nicht alle Webseiten erlauben es, ihre Inhalte auf diese Weise herunterzuladen, und manche haben möglicherweise auch Sicherheitsmaßnahmen, die das Herunterladen mit JavaScript verhindern. Es ist daher immer wichtig, die Nutzungsbedingungen einer Webseite zu prüfen, bevor man sie herunterlädt.

## Siehe auch

- [XMLHttpRequest on MDN](https://developer.mozilla.org/de/docs/Web/API/XMLHttpRequest)
- [Fetch API on MDN](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)
- [Puppeteer on GitHub](https://github.com/puppeteer/puppeteer)
- [Cheerio on GitHub](https://github.com/cheeriojs/cheerio)