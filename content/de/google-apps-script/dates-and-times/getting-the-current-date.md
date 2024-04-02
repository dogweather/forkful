---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:30.517120-07:00
description: "Das Abrufen des aktuellen Datums in Google Apps Script handelt vom Abrufen\
  \ des aktuellen Datums und der Uhrzeit, einer g\xE4ngigen Aufgabe zur\u2026"
lastmod: '2024-03-13T22:44:53.345482-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in Google Apps Script handelt vom Abrufen\
  \ des aktuellen Datums und der Uhrzeit, einer g\xE4ngigen Aufgabe zur\u2026"
title: Das aktuelle Datum abrufen
weight: 29
---

## Was & Warum?

Das Abrufen des aktuellen Datums in Google Apps Script handelt vom Abrufen des aktuellen Datums und der Uhrzeit, einer gängigen Aufgabe zur Automatisierung von Aufgaben, Protokollierung und Zeitstempelung in Apps, die an das Google-Ökosystem gebunden sind. Programmierer verwenden dies für die Generierung dynamischer Inhalte, das Verfolgen von Fristen und die Planung innerhalb von Google Docs, Sheets und anderen Google-Diensten.

## Wie geht das:

Google Apps Script, das auf JavaScript basiert, bietet einfache Methoden, um das aktuelle Datum zu erhalten. Sie können den Konstruktor `new Date()` verwenden, um ein neues Datumsobjekt zu erstellen, das das aktuelle Datum und die Uhrzeit darstellt. So können Sie dies auf verschiedene Weisen manipulieren und anzeigen.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Protokolliert das aktuelle Datum und die Uhrzeit in der Zeitzone des Skripts
  
  // Um nur das Datum im YYYY-MM-DD-Format anzuzeigen
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Beispiel-Ausgabe: "2023-04-01"
  
  // Anzeige in einem leichter lesbaren Format
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Beispiel-Ausgabe: "1. April 2023, 12:00:00 PM GMT+1"
}
```

Diese Schnipsel zeigen, wie man das aktuelle Datum und die Uhrzeit aufnimmt und formatiert, und zeigen die Vielseitigkeit für verschiedene Programmieraufgaben innerhalb von Google Apps Script.

## Tiefergehend

Bevor JavaScript sich auf das `Date`-Objekt festlegte, mussten Programmierer die Zeit und das Datum manuell durch weniger standardisierte und umständlichere Mittel verfolgen. Dies umfasste die Verwendung von Zeitstempel-Integern und selbstgemachten Datumsfunktionen, die von einer Programmierumgebung zur anderen variierten und zu Inkonsistenzen und Kompatibilitätsproblemen führten.

Die Einführung des `new Date()`-Objekts in JavaScript und somit auch in Google Apps Script standardisierte Datum- und Zeitoperationen, machte sie intuitiver und reduzierte die für datumsbezogene Operationen notwendige Code-Menge. Es ist erwähnenswert, dass, obwohl die Implementierung von Google Apps Script bequem und für viele Anwendungen innerhalb der Produktpalette von Google ausreichend ist, sie möglicherweise nicht allen Szenarien gerecht wird, insbesondere solchen, die eine komplexe Zeitzonenbehandlung oder präzise Zeitstempelprotokollierung in schnelllebigen Umgebungen erfordern.

Für solche fortgeschrittenen Anwendungsfälle wenden sich Programmierer oft an Bibliotheken wie Moment.js oder date-fns in JavaScript. Obwohl Google Apps Script diese Bibliotheken nicht nativ unterstützt, können Entwickler einige ihrer Funktionen mit verfügbaren JavaScript-Datums Methoden nachahmen oder auf externe Bibliotheken über den HTML-Service oder den URL-Abholservice von Apps Script zugreifen. Trotz dieser Alternativen bleibt die Einfachheit und Integration der nativen Datums- und Zeitfunktionen von Google Apps Script die erste Wahl für die meisten Aufgaben im Google-Ökosystem.
