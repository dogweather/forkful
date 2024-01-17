---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Javascript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Berechnen von Daten in der Zukunft oder Vergangenheit ist eine häufige Aufgabe in der Programmierung. Es handelt sich dabei um die Manipulation von Datumsangaben, um ein bestimmtes Datum oder eine bestimmte Zeit für einen bestimmten Zweck zu erhalten. Programmierer verwenden dies, um beispielsweise Kalender- oder Zeitfunktionen zu erstellen oder um zeitgesteuerte Aufgaben in ihren Programmen zu automatisieren.

## Wie geht's?

```Javascript 
// Beispiel für die Berechnung eines Datums in der Zukunft
const today = new Date();
const futureDate = new Date();
futureDate.setDate(today.getDate() + 7); // Fügt 7 Tage hinzu
console.log(futureDate); // Ausgabe: 2021-05-28T00:00:00.000Z

// Beispiel für die Berechnung eines Datums in der Vergangenheit
const today = new Date();
const pastDate = new Date();
pastDate.setDate(today.getDate() - 10); // Subtrahiert 10 Tage
console.log(pastDate); // Ausgabe: 2021-05-11T00:00:00.000Z
```

## Tiefere Einblicke

Die genaue Implementierung der Datumsberechnung hängt von der Programmiersprache und der verwendeten Bibliothek ab. In der Vergangenheit mussten Entwickler manchmal komplexe Berechnungen durchführen, um ein Datum zu manipulieren. Mit der Einführung von modernen Datums- und Zeitfunktionen wie JavaScript's ```Date``` Objekt, ist es jedoch viel einfacher geworden, Daten in der Zukunft oder Vergangenheit zu berechnen.

Es gibt auch alternative Ansätze, die in bestimmten Fällen nützlich sein können, wie zum Beispiel die Verwendung von Zeitstempeln anstelle von Datumsangaben oder die Verwendung von Bibliotheken für spezifische Datums- und Zeitmanipulationen.

## Siehe auch

- [Mozilla Developer Network: Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: JavaScript Date Objects](https://www.w3schools.com/js/js_date_objects.asp)
- [Moment.js](https://momentjs.com/) - Eine beliebte JavaScript Bibliothek für das Manipulieren von Datums- und Zeitangaben.