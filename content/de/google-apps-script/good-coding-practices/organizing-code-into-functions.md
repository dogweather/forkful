---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:09.771796-07:00
description: "Wie: In Google Apps Script, das auf JavaScript basiert, definieren Sie\
  \ Funktionen mit dem Schl\xFCsselwort `function`, gefolgt von einem einzigartigen\u2026"
lastmod: '2024-03-13T22:44:53.339964-06:00'
model: gpt-4-0125-preview
summary: "In Google Apps Script, das auf JavaScript basiert, definieren Sie Funktionen\
  \ mit dem Schl\xFCsselwort `function`, gefolgt von einem einzigartigen Funktionsnamen,\
  \ Klammern `()` die Parameter enthalten k\xF6nnen, und geschweiften Klammern `{}`,\
  \ die den Codeblock der Funktion umschlie\xDFen."
title: Organisation von Code in Funktionen
weight: 18
---

## Wie:
In Google Apps Script, das auf JavaScript basiert, definieren Sie Funktionen mit dem Schlüsselwort `function`, gefolgt von einem einzigartigen Funktionsnamen, Klammern `()` die Parameter enthalten können, und geschweiften Klammern `{}`, die den Codeblock der Funktion umschließen. Hier ist ein grundlegendes Beispiel:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Hallo, ' + user + '!');
}

greetUser();
```

Beispielausgabe:

```
Hallo, jemand@example.com!
```

Betrachten wir jetzt ein praktischeres Beispiel im Zusammenhang mit Google Sheets, bei dem wir die Funktionalität in zwei Funktionen aufteilen: eine zum Einrichten des Blatts und eine andere zum Befüllen mit Daten.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Verkaufsdaten');
  sheet.appendRow(['Artikel', 'Menge', 'Preis']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Verkaufsdaten');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// Initialisiere Datenarray
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// Führe die Funktionen aus
setupSheet();
populateSheet(salesData);
```

In diesem Beispiel bereitet `setupSheet` das Blatt vor, und `populateSheet` verwendet ein Array von Verkaufsdaten, um das Blatt zu befüllen. Diese Aufteilung der Anliegen macht den Code sauberer und anpassungsfähiger an Veränderungen.

## Tiefergehend
Das Konzept, Code in Funktionen aufzuteilen, ist nicht neu oder einzigartig für Google Apps Script; es ist eine grundlegende Programmierpraxis, die in fast allen Programmiersprachen befürwortet wird. Historisch gesehen entwickelten sich Funktionen aus dem mathematischen Konzept, Eingaben auf Ausgaben abzubilden, was zu einem Eckpfeiler der strukturierten Programmierung wurde. Dieser Ansatz fördert die Modularität und Wiederverwendung von Code und bietet klare Wege für das Testen einzelner Teile des Skripts.

Google Apps Script profitiert erheblich von den erstklassigen Funktionen von JavaScript, die es ermöglichen, Funktionen als Argumente zu übergeben, von anderen Funktionen zurückzugeben und Variablen zuzuweisen. Diese Funktion eröffnet fortgeschrittene Muster wie Callbacks und funktionale Programmierung, obwohl diese Muster eine Komplexität einführen können, die für einfache Automatisierungsaufgaben in Google Apps Script unnötig sein mag.

Für größere Projekte oder komplexere Anwendungen könnten Entwickler die neueren Funktionen von JavaScript wie Pfeilfunktionen, async/await für asynchrone Operationen und sogar TypeScript für statische Typisierung erforschen. TypeScript kann insbesondere so kompiliert werden, dass es als Google Apps Script ausgeführt wird und bietet Entwicklern, die eine robustere Typüberprüfung und fortgeschrittene objektorientierte Funktionen suchen, eine Möglichkeit.

Allerdings bietet für die meisten Skriptbedürfnisse innerhalb der Google Apps Suite das Festhalten an einfachen, gut organisierten Funktionen, wie demonstriert, eine solide Grundlage. Es ist immer ein Balanceakt, fortgeschrittene Funktionen für Effizienz zu nutzen und gleichzeitig Einfachheit für einfache Wartung und Lesbarkeit zu wahren.
