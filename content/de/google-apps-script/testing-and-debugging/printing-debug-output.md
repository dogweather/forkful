---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:17.600768-07:00
description: "Wie: Google Apps Script bietet die `Logger`-Klasse f\xFCr grundlegendes\
  \ Debugging und f\xFCr fortgeschrittenere Bed\xFCrfnisse die `console`-Klasse, die\
  \ in der\u2026"
lastmod: '2024-03-13T22:44:53.336861-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script bietet die `Logger`-Klasse f\xFCr grundlegendes Debugging\
  \ und f\xFCr fortgeschrittenere Bed\xFCrfnisse die `console`-Klasse, die in der\
  \ V8-Laufzeit eingef\xFChrt wurde."
title: Debug-Ausgabe drucken
weight: 33
---

## Wie:
Google Apps Script bietet die `Logger`-Klasse für grundlegendes Debugging und für fortgeschrittenere Bedürfnisse die `console`-Klasse, die in der V8-Laufzeit eingeführt wurde.

**Logger verwenden:**

Die Logger-Klasse ermöglicht es Ihnen, Debug-Nachrichten zu protokollieren, die Sie nach der Ausführung im Apps Script-Editor unter `Ansicht > Protokolle` anzeigen können. Hier ist ein einfaches Beispiel:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hallo, %s!", name);
}
```

Nach dem Ausführen von `logSample()`, können Sie das Protokoll mit "Hallo, Wired Reader!" im Protokoll-Viewer ansehen.

**console.log mit der V8-Laufzeit verwenden:**

Mit der V8-Laufzeit bietet `console.log` eine den Entwicklern aus anderen Sprachen vertrautere Syntax:

```javascript
function consoleSample() {
  var status = 'aktiv';
  var count = 150;
  console.log(`Aktueller Status: ${status}, Anzahl: ${count}`);
}
```

Nach der Ausführung greifen Sie auf das Stackdriver-Protokollieren unter `Ansicht > Stackdriver-Protokollierung` zu, um die Ausgabe anzusehen. Es ist leistungsfähiger, unterstützt String-Interpolation und Objektinspektion und integriert sich mit Googles Cloud-Logging, das persistente Protokolle und erweiterte Filterfähigkeiten bietet.

**Beispielausgabe von console.log:**

```
Aktueller Status: aktiv, Anzahl: 150
```

## Tiefergehende Betrachtung
Ursprünglich war `Logger.log` das primäre Werkzeug für das Debugging in Google Apps Script und bot eine einfache, unkomplizierte Möglichkeit, Ausgaben zur Überprüfung auszudrucken. Jedoch, als Skripte komplexer wurden und stärker mit Google Cloud Platform-Diensten integriert wurden, wurde der Bedarf an einer robusteren Protokollierungslösung offensichtlich.

Hier kommt die V8-Laufzeit ins Spiel und bringt `console.log` mit. Dies bringt Google Apps Script nicht nur in Einklang mit der standardmäßigen JavaScript-Syntax, macht die Sprache zugänglicher für Entwickler, die mit JavaScript vertraut sind, sondern nutzt auch die leistungsfähige Infrastruktur von Googles Cloud-Logging-Fähigkeiten. Die Einführung von `console.log` und seine Integration mit der Google Cloud Platform markiert eine signifikante Evolution in den Debugging-Fähigkeiten innerhalb von Google Apps Script und bietet Entwicklern einen dynamischeren und skalierbareren Ansatz zur Überwachung und Fehlerbehebung ihrer Skripte.

Während `Logger.log` für grundlegende Debugging-Bedürfnisse und kleine Projekte ausreicht, bietet `console.log` mit der V8-Laufzeit eine umfassendere und zukunftssichere Lösung. Dies beinhaltet die Fähigkeit, Protokolle über die Ausführungssitzung hinaus zu behalten, Protokolle innerhalb der Google Cloud-Konsole zu suchen und zu filtern, sowie die allgemeine Ausrichtung an modernen JavaScript-Entwicklungspraktiken. Entwickler sollten jedoch ihre Bedürfnisse gegen die Komplexität und den Umfang ihrer Projekte abwägen, wenn sie zwischen diesen Optionen wählen.
