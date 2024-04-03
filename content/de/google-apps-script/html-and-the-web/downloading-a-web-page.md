---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:45.024696-07:00
description: "Wie: In Google Apps Script ist der `UrlFetchApp`-Dienst zentral f\xFC\
  r das Herunterladen von Webinhalten. Unten finden Sie eine Schritt-f\xFCr-Schritt-Anleitung\u2026"
lastmod: '2024-03-13T22:44:53.332499-06:00'
model: gpt-4-0125-preview
summary: "In Google Apps Script ist der `UrlFetchApp`-Dienst zentral f\xFCr das Herunterladen\
  \ von Webinhalten."
title: Herunterladen einer Webseite
weight: 42
---

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
