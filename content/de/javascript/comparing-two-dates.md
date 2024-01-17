---
title:                "Vergleichen von zwei Daten"
html_title:           "Javascript: Vergleichen von zwei Daten"
simple_title:         "Vergleichen von zwei Daten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Vergleichen von zwei Daten ist ein häufiges Szenario in der Programmierung. Es erfordert die Analyse von zwei Datumsobjekten, um festzustellen, ob sie gleich sind, eines größer als das andere oder in einer bestimmten Reihenfolge stehen. Programmierer tun dies, um beispielsweise Ereignisse innerhalb eines bestimmten Zeitraums zu filtern oder zu sortieren.

## Wie geht's?

```Javascript
// Beispiel 1: Vergleichen von Datum Objekten
let date1 = new Date('09/18/2020');
let date2 = new Date('09/21/2020');

// Überprüfen, ob date1 und date2 gleich sind
if (date1.getTime() === date2.getTime()) {
  console.log('date1 und date2 sind gleich.');
} else {
  console.log('date1 und date2 sind unterschiedlich.');
}
// Ausgabe: date1 und date2 sind unterschiedlich.

// Beispiel 2: Überprüfen, ob ein Datum innerhalb eines bestimmten Zeitraums liegt
let currentDate = new Date();
let startDate = new Date('01/01/2020');
let endDate = new Date('12/31/2020');

if (startDate.getTime() < currentDate.getTime() && currentDate.getTime() < endDate.getTime()) {
  console.log('currentDate liegt innerhalb des Zeitraums.');
} else {
  console.log('currentDate liegt nicht innerhalb des Zeitraums.');
}
// Ausgabe: currentDate liegt innerhalb des Zeitraums.
```

## Tiefere Einblicke

Das Vergleichen von zwei Daten hat eine lange Historie in der Programmierung. Früher mussten Programmierer komplexe Berechnungen durchführen, um Datumsunterschiede festzustellen. Mit der Einführung von Datum Objekten und den dazugehörigen Methoden in JavaScript ist das Vergleichen von Daten jedoch viel einfacher geworden.

Alternativen zu dieser Methode sind das Vergleichen von Datumszeichenfolgen oder die Verwendung von Bibliotheken wie Moment.js. Bei der Implementierung ist zu beachten, dass Datum Objekte in JavaScript von der lokalen Zeitzone des Benutzers abhängig sind.

## Siehe auch

- [Date Objekt in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Bibliothek](https://momentjs.com/)
- [Vergleichen von Datum Objekten in JavaScript](https://www.smashingmagazine.com/2012/08/javascript-date-manipulation/)