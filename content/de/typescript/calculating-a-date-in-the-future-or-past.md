---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "TypeScript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine häufige Aufgabe für Programmierer. Diese Funktion ermöglicht es uns, das heutige Datum um eine bestimmte Anzahl von Tagen, Monaten oder Jahren zu ändern. Dies kann nützlich sein, um beispielsweise Fälligkeitsdaten für Aufgaben oder Ereignisse zu berechnen.

## Wie geht's:
Codebeispiele und Beispiel-Ausgaben in ```TypeScript ...``` Codeblöcken.

```TypeScript
// Beispiel für ein Datum in der Zukunft berechnen
const heute = new Date(); // Heutiges Datum 
console.log(heute); // Ausgabe: 2021-03-24T14:19:37.840Z

heute.setDate(heute.getDate() + 7); // Heutiges Datum um 7 Tage ändern
console.log(heute); // Ausgabe: 2021-03-31T14:19:37.840Z
```

```TypeScript
// Beispiel für ein Datum in der Vergangenheit berechnen 
const heute = new Date(); // Heutiges Datum
console.log(heute); // Ausgabe: 2021-03-24T14:19:37.840Z

heute.setFullYear(heute.getFullYear() - 5); // Heutiges Datum auf vor 5 Jahren setzen
console.log(heute); // Ausgabe: 2016-03-24T14:19:37.840Z
```

## Tief tauchen:
Die Berechnung von Datumswerten hat eine lange Geschichte und ist in vielen verschiedenen Programmiersprachen implementiert. In TypeScript können wir verschiedene Methoden der Date-Klasse verwenden, um bestimmte Zeiteinheiten (Tage, Monate, Jahre) zu ändern. Alternativ können wir auch externe Bibliotheken wie Moment.js verwenden, um erweiterte Funktionen für das Manipulieren von Datumswerten zu nutzen.

## Sieh auch:
- [Moment.js Dokumentation](https://momentjs.com/docs/)
- [Date Klasse in TypeScript](https://www.typescriptlang.org/docs/handbook/2/functions.html#the-date-type)