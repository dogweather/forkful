---
title:                "Gleam: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Die grundlegende Aufgabe des Web Crawling ist das Herunterladen von Webseiten und das Extrahieren von Informationen. Web Crawling ist nützlich für verschiedene Zwecke wie Datenanalyse, Marktforschung und SEO-Optimierung. Das Herunterladen von Webseiten ist der erste Schritt in diesem Prozess und ermöglicht es Programmierern, wichtige Daten von verschiedenen Websites zu erhalten.

## Wie es geht

Um eine Webseite mit Gleam herunterzuladen, können Sie den `httpc`-Client verwenden. Der Befehl `httpc.get` ermöglicht es Ihnen, eine URL anzugeben und die HTML-Inhalte der Webseite zu erhalten. Hier ist ein Beispielcode, der eine Webseiten-URL und die Ausgabe des HTML-Codes enthält:

```Gleam
let response = httpc.get("https://www.example.com")
import gleam/html
import gleam/http
let html_code = case response {
  Ok(http.Success(body)) -> body
  Err(err) -> html.p([html.text(err)])
}

html_code
```

Die Ausgabe des obigen Codes zeigt den HTML-Code der Webseite in Ihrem Terminal an. Sie können diese Seite auch als Variable speichern und sie später analysieren oder speichern.

## Tiefer Einblick

Beim Herunterladen einer Webseite gibt es einige Faktoren, die beachtet werden müssen, um ein effektives Web Crawling zu erreichen. Ein wichtiger Faktor ist die Verarbeitung von Cookies. Gleam bietet eine integrierte Cookie-Verwaltung, um sicherzustellen, dass Sie die gleichen Informationen erhalten, die Sie normalerweise auf der Webseite sehen würden.

Ein weiterer wichtiger Faktor ist die Behandlung von Redirects. Es ist üblich, dass Webseiten automatisch auf eine andere Seite weitergeleitet werden. Gleam ermöglicht es Ihnen, diese Weiterleitungen automatisch zu verfolgen und die eigentliche Seite zu erhalten, auf die Sie zugreifen möchten.

Eine weitere Option ist die Verwendung von `httpc.request` anstelle von `httpc.get`. Mit dieser Methode können Sie die Request-Optionen genauer definieren, wie z.B. das Festlegen von benutzerdefinierten Headern oder die Verwendung von Authentifizierungsdaten.

## Siehe auch

- [Gleam-Dokumentation für Web Crawling](https://gleam.run/books/httpc) 
- [Web Crawling mit Gleam](https://gleam.run/articles/web-crawling) 
- [Gleam-Codebeispiele für Web Crawling](https://github.com/gleam-lang/gleam/search?q=web+crawl&type=code)