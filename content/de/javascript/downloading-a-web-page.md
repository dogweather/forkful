---
title:                "Javascript: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen von Webseiten ist ein wichtiger Teil der Webentwicklung. Es ermöglicht uns, Daten von einer Webseite zu extrahieren und in unserer eigenen Anwendung zu verwenden. Außerdem können wir damit Fehler auf einer Webseite finden und so unsere Programmierkenntnisse verbessern.

## Wie man Webseiten herunterlädt

Um eine Webseite herunterzuladen, können wir verschiedene Techniken verwenden. Eine einfache Möglichkeit ist die Verwendung der `fetch` API in JavaScript. Hier ist ein Beispielcode, der eine Webseite herunterlädt und die HTML-Inhalte ausgibt:

```
fetch('https://www.example.com')
  .then(response => response.text())
  .then(html => console.log(html))
  .catch(error => console.log(error));
```

Dieser Code verwendet die `fetch` Funktion, um eine HTTP-Anfrage an die angegebene URL zu senden. Dann verwenden wir `response.text()` um den HTML-Inhalt der Webseite als Text zu erhalten. Diesen Text geben wir dann über `console.log` aus. Falls ein Fehler auftritt, verwenden wir `catch` um ihn zu behandeln.

## Tiefergehende Informationen

Es gibt viele weitere Möglichkeiten, um Webseiten herunterzuladen. Einige davon sind die Verwendung von Bibliotheken wie `axios` oder `request` in Node.js oder das Parsen von HTML-Inhalten mit einem HTML-Parser wie `cheerio`. Es ist auch wichtig zu verstehen, wie HTTP-Anfragen und -Antworten funktionieren und wie wir Cookies und Header in unseren Anfragen verwenden können.

Beim Herunterladen von Webseiten müssen wir auch darauf achten, dass wir die Nutzungsbedingungen der Webseite einhalten und keine unerwünschten Auswirkungen haben, wie zum Beispiel zu viele Anfragen auf einmal zu senden.

## Siehe auch

- [Dokumentation der fetch API](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)
- [Node.js Dokumentation für HTTP-Anfragen](https://nodejs.org/api/http.html)
- [Cheerio HTML-Parser](https://github.com/cheeriojs/cheerio)