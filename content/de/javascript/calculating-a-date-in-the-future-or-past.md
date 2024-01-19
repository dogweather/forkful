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

Das Berechnen eines zukünftigen oder vergangenen Datums ist eine übliche Aufgabe in der Programmierung. Mit dieser Berechnung können wir Ereignisse zeitlich planen oder Analysen auf Zeitreihendaten durchführen.

## So geht's:

Betrachten wir zunächst, wie man ein zukünftiges Datum berechnet. Wir verwenden dazu date-Objekte und die Methode setDate(). 

```Javascript
let heute = new Date(); 
heute.setDate(heute.getDate() + 5); 
console.log(heute);
```
Und um ein vergangenes Datum zu berechnen, subtrahieren wir die Tage.

```Javascript
let heute = new Date(); 
heute.setDate(heute.getDate() - 5); 
console.log(heute);
```
Die Ausgabe wird das Datum von heute plus bzw. minus fünf Tage sein. 

## Tiefer eintauchen:

Der JavaScript-Standard ECMA-262 definiert die Date-Objekte. Es gibt Alternativen zu den nativen JavaScript-Datumsmethoden, wie bibliotheken wie Moment.js oder date-fns, die oft benutzerfreundlicher sind und mehr Funktionen bieten.

Die Date-Objektmethoden basieren auf dem Wert, der den Zeitabstand in Millisekunden seit dem 1. Januar 1970 00:00:00 UTC darstellt. Beim Hinzufügen oder Subtrahieren von Tagen wird dieser Millisekundenwert entsprechend erhöht oder verringert und dann in ein Datum umgewandelt.

## Weiterführende Links:

- JavaScript Date Objekte: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/