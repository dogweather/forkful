---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Verfügt man über zwei Daten (Datum und Uhrzeit), ist es meist notwendig, diese miteinander zu vergleichen. Programmierer tun dies, um Ereignisse zu planen, Zeitspannen zu berechnen oder Reihenfolgen zu erstellen - ziemlich alltägliche Aufgaben in der Softwareentwicklung.

## So geht's:

Vergleich von zwei Daten in TypeScript ist recht einfach. Schauen wir uns das anhand von Beispielen an.

```TypeScript
let datum1 = new Date('2021-12-01');
let datum2 = new Date('2022-01-01');

if (datum1 > datum2) {
    console.log("Datum1 ist später als Datum2.");
} else if (datum1 < datum2) {
    console.log("Datum1 ist früher als Datum2.");
} else {
    console.log("Datum1 und Datum2 sind gleich.");
}
```
In der Ausgabe von oben sehen wir "Datum1 ist früher als Datum2." Das liegt daran, dass das '2021-12-01' vor '2022-01-01' ist.

## Vertiefung:

Historisch gesehen hat JavaScript, die Basis von TypeScript, diese eingebaute Date-Objekt-Klasse schon immer gehabt. Dieselbe Methodologie wird auch in TypeScript angewendet.

Es gibt mehrere Methoden, um zwei Daten zu vergleichen, darunter: den Zeitstempel zu vergleichen, die native JavaScript-Datei-Objektvergleichmethode zu verwenden, oder Bibliotheken wie Moment.js zu verwenden, wenn man mit komplexer Zeitmanipulation zu tun hat.

Aus Implementierungssicht gibt TypeScript den Entwicklern mehr Typensicherheit beim Umgang mit Daten und Zeiten, da es statische Typprüfungen und Code-Vervollständigungsfunktionen bietet, die in nativem JavaScript nicht vorhanden sind.

## Weiterführende Links:

- [Mozilla Developer Network - Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - Display](https://momentjs.com/docs/#/displaying/)
- [TypeScript - Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html)