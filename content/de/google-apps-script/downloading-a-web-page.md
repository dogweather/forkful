---
title:                "Herunterladen einer Webseite"
aliases:
- de/google-apps-script/downloading-a-web-page.md
date:                  2024-02-01T21:52:45.024696-07:00
model:                 gpt-4-0125-preview
simple_title:         "Herunterladen einer Webseite"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite in Google Apps Script beinhaltet das Abrufen des Inhalts einer Webseite über HTML für verschiedene Zwecke, wie zum Beispiel Web Scraping, Datenextraktion oder die Überwachung von Änderungen. Programmierer wählen diesen Vorgang, um Datensammel- oder Integrationstätigkeiten zu automatisieren, manuellen Aufwand zu minimieren und eine Echtzeit-Datenverarbeitung sicherzustellen.

## Wie:

In Google Apps Script ist der `UrlFetchApp`-Dienst zentral für das Herunterladen von Webinhalten. Unten finden Sie eine Schritt-für-Schritt-Anleitung und ein einfaches Beispiel, das zeigt, wie man den HTML-Inhalt einer Webseite abruft und protokolliert:

1. **Grundlegende Abrufoperation:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Dieser Code ruft den HTML-Inhalt von example.com ab und protokolliert ihn. Es ist eine unkomplizierte Darstellung davon, wie man den Quellcode einer Webseite ohne zusätzliche Parameter erhält.

2. **Umgang mit Weiterleitungen und HTTPS:**

Für HTTPS oder den Umgang mit Weiterleitungen bleibt der Code weitgehend gleich, aber erwägen Sie die Implementierung einer Fehlerbehandlung oder spezifische Optionen für Weiterleitungen:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Automatisches Folgen von Weiterleitungen
    'muteHttpExceptions': true // Mögliche Ausnahmen stumm schalten, um sie elegant zu behandeln
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Ratenbeschränkungen und Kontingente:**

Achten Sie auf die Kontingente von Google Apps Script; eine intensive Nutzung erfordert möglicherweise eine Fehlerbehandlung für Ratenbeschränkungen.

## Vertiefung

Historisch gesehen begann der Download und die Manipulation von Webinhalten mit einfachen HTTP-Anfragen, die sich erheblich mit dem Aufkommen von Skriptsprachen weiterentwickelten. Google Apps Script ermöglicht die einfache Ausführung solcher Aufgaben innerhalb des G Suite-Ökosystems und nutzt dabei Googles robuste Infrastruktur. Der `UrlFetchApp`-Dienst ist ein Kernbestandteil dieser Funktionalität und fasst komplexe HTTP/S-Anfragen in eine einfachere, anwendungsspezifische Schnittstelle zusammen.

Trotz seiner Bequemlichkeit ist Google Apps Script möglicherweise nicht immer das beste Werkzeug für intensives Web Scraping oder wenn eine komplexe Nachbearbeitung der abgerufenen Daten aufgrund von Ausführungszeitbeschränkungen und Kontingenten, die von Google auferlegt sind, erforderlich ist. In solchen Fällen könnten spezielle Web-Scraping-Frameworks oder für asynchrone I/O-Operationen konzipierte Sprachen wie Node.js mit Bibliotheken wie Puppeteer oder Cheerio mehr Flexibilität und Leistung bieten.

Darüber hinaus ist Google Apps Script zwar ein hervorragendes Werkzeug für die Integration mit Google-Diensten (wie Sheets, Docs und Drive) und für leichte Datenabrufoperationen, es ist jedoch entscheidend, die Beschränkungen der Ausführungsumgebung zu beachten. Für intensive Aufgaben sollten Sie die Verwendung von Google Cloud Functions oder fortgeschrittenen Diensten von Apps Script mit externen Rechenressourcen für die Verarbeitung in Betracht ziehen.
