---
date: 2024-01-20 17:31:11.852953-07:00
description: 'How to: JavaScript bietet eingebaute Funktionen, um mit Daten zu arbeiten.
  Hier zwei Beispiele.'
lastmod: '2024-03-13T22:44:54.279530-06:00'
model: gpt-4-1106-preview
summary: JavaScript bietet eingebaute Funktionen, um mit Daten zu arbeiten.
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

## How to:
JavaScript bietet eingebaute Funktionen, um mit Daten zu arbeiten. Hier zwei Beispiele:

```javascript
// Datum heute plus 5 Tage
let heute = new Date();
let zukunft = new Date(heute.getTime() + (5 * 24 * 60 * 60 * 1000));
console.log(zukunft); // Ausgabe: Datum in 5 Tagen

// Datum heute minus 30 Tage
let vergangenheit = new Date(heute.getTime() - (30 * 24 * 60 * 60 * 1000));
console.log(vergangenheit); // Ausgabe: Datum vor 30 Tagen
```

## Deep Dive
Historisch gesehen wurde Datum-Berechnungen oft manuell gemacht – mit viel Potential für Fehler. JavaScript erleichtert dies enorm, doch Zeitzone und Schaltjahre bleiben Stolpersteine.

Alternativen: Bibliotheken wie Moment.js oder date-fns bieten Funktionen mit mehr Optionen und besserer Lesbarkeit.

Implementierung: JavaScript zählt Zeit in Millisekunden seit dem 1. Januar 1970 (Unix-Zeit). Rechne mit Millisekunden und achte auf Zeitzone und Sommer-/Winterzeit.

## See Also
- MDN Web Docs: [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- JavaScript Info: [Date and time](https://javascript.info/date)
- Date-fns Library: [date-fns](https://date-fns.org/)
