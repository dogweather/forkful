---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:25.510424-07:00
description: "Das Parsen eines Datums aus einem String beinhaltet die Umwandlung von\
  \ Text, der ein Datum darstellt, in ein Datum-Objekt, was Programmierern erm\xF6\
  glicht,\u2026"
lastmod: '2024-03-13T22:44:53.344345-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String beinhaltet die Umwandlung von Text,\
  \ der ein Datum darstellt, in ein Datum-Objekt, was Programmierern erm\xF6glicht,\
  \ datumsbezogene Operationen wie Vergleiche, Arithmetik und Formatierung durchzuf\xFC\
  hren."
title: Einen Datum aus einem String auslesen
weight: 30
---

## Wie zu:
In Google Apps Script, welches auf JavaScript basiert, gibt es mehrere Ansätze, um ein Datum aus einem String zu parsen. Unten finden Sie Beispiele, die sowohl native JavaScript-Methoden als auch Google Apps Script-Dienstprogramme verwenden.

**Verwendung des `new Date()` Konstruktors:**

Die einfachste Art, einen String in Google Apps Script in ein Datum zu parsen, erfolgt mit dem Konstruktor des `Date` Objekts. Es erfordert jedoch, dass der Datumsstring in einem von der Methode Date.parse() erkannten Format vorliegt (z.B. JJJJ-MM-TT).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Loggt Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**Verwendung von `Utilities.parseDate()`:**

Für mehr Flexibilität, insbesondere bei benutzerdefinierten Datumsformaten, bietet Google Apps Script `Utilities.parseDate()`. Diese Methode erlaubt es Ihnen, das Datumsformat, die Zeitzone und die Gebietsschema zu spezifizieren.

```javascript
const dateString = '01-04-2023'; // TT-MM-JJJJ
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // Loggt Sat Apr 01 2023 00:00:00 GMT+0000 (UTC) abhängig von der Zeitzone des Skripts
```

Hinweis: Während `Utilities.parseDate()` mehr Kontrolle bietet, kann sein Verhalten basierend auf der Zeitzone des Skripts variieren, daher ist es entscheidend, die Zeitzone explizit anzugeben, wenn Ihre Anwendung mit Daten aus verschiedenen Regionen arbeitet.

## Vertiefung
Das Parsen von Daten in Programmiersprachen war historisch gesehen mit Herausforderungen verbunden, hauptsächlich wegen der Vielfalt an Datumsformaten und der Komplexität von Zeitzonen. Der Ansatz von Google Apps Script, der hauptsächlich von JavaScript abgeleitet ist, zielt darauf ab, dies zu vereinfachen, indem sowohl das einfache `Date` Objekt als auch die vielseitigere Funktion `Utilities.parseDate()` angeboten werden. Jede Methode hat jedoch ihre Einschränkungen; beispielsweise führt das Verlassen auf den `Date` Konstruktor mit Strings zu Inkonsistenzen in verschiedenen Umgebungen aufgrund unterschiedlicher Interpretationen von Datumsformaten. Andererseits erfordert `Utilities.parseDate()` ein klareres Verständnis des Formats, der Zeitzone und des Gebietsschemas, was es etwas komplexer macht, aber zuverlässiger für spezifische Bedürfnisse.

Alternative Bibliotheken oder Dienste, wie Moment.js (empfiehlt nun Luxon für neue Projekte), bieten umfangreichere Funktionalitäten und eine bessere Zeitzonenverwaltung und adressieren viele dieser Herausforderungen. Doch im Kontext von Google Apps Script, wo externe Bibliotheken Einschränkungen haben, wird das Verständnis und die effektive Nutzung der integrierten Methoden entscheidend. Programmierer, die aus anderen Sprachen kommen, finden möglicherweise die Nuancen der Datumsverwaltung in Google Apps Script einzigartig herausfordernd, können aber mit einem tiefen Verständnis der verfügbaren Werkzeuge und sorgfältiger Berücksichtigung des globalen Charakters ihrer Anwendungen eine robuste Datumsverarbeitung erreichen.
