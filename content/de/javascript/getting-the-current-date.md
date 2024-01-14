---
title:    "Javascript: Das aktuelle Datum erhalten"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum
Vielleicht hast du dich schon einmal gefragt, warum Programmiererinnen und Programmierer sich überhaupt mit dem aktuellen Datum befassen. Die Antwort ist einfach: Das Datum ist eine häufig verwendete Angabe in vielen Anwendungen und spielt daher eine wichtige Rolle in der Programmierung.

## Wie geht das?
Um das aktuelle Datum in deinem JavaScript Code zu bekommen, kannst du die integrierte `Date()` Funktion verwenden. Diese gibt ein Objekt mit dem aktuellen Datum zurück, welches du dann weiter verarbeiten kannst.

```Javascript
var datum = new Date();
console.log(datum);

// Output: Tue Apr 20 2021 13:35:26 GMT+0200 (Central European Summer Time)
```

Du kannst nun verschiedene Methoden anwenden, um bestimmte Teile des Datums auszulesen. Zum Beispiel könntest du nur den aktuellen Tag oder Monat ausgeben lassen.

```Javascript
var monat = datum.getMonth();
console.log("Aktueller Monat: " + monat);

// Output: Aktueller Monat: 3 (da die Monate bei Date() mit 0 beginnen)
```

## Tiefere Informationen
Die `Date()` Funktion hat viele nützliche Methoden, um mit dem Datum zu arbeiten. Einige davon sind:

- `getDate()` gibt den aktuellen Tag im Monat zurück.
- `getFullYear()` gibt das aktuelle Jahr zurück.
- `getHours()` gibt die aktuelle Stunde zurück.

Du kannst auch das Datum in verschiedenen Formaten ausgeben lassen, zum Beispiel als String oder mit bestimmten Trennzeichen.

```Javascript
console.log(datum.toDateString());
// Output: Tue Apr 20 2021

console.log(datum.toLocaleDateString('de-DE'));
// Output: 20.04.2021
```

Es gibt auch die Möglichkeit, das Datum zu manipulieren, zum Beispiel um einen Tag hinzuzufügen oder zu subtrahieren.

```Javascript
var zukunftsDatum = datum.setDate(datum.getDate() + 5);
console.log(zukunftsDatum);

// Output: 1619013326821 (das Datum in Millisekunden seit dem 1. Januar 1970)
```

## Siehe auch
- [Date() Dokumentation von MDN](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Video Tutorial auf YouTube (auf Deutsch)](https://www.youtube.com/watch?v=3PtWNzSak5c)
- [Moment.js - eine JavaScript Bibliothek für das Arbeiten mit Datum und Zeit](https://momentjs.com/)