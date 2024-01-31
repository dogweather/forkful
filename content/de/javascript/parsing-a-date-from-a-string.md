---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:37:08.021525-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, die textuellen Informationen eines Datums in ein Date-Objekt umzuwandeln. Das brauchen Programmierer, um Datumsangaben zu verarbeiten, zu vergleichen und in verschiedenen Formaten anzuzeigen.

## How to:
Um ein Datum aus einem String in JavaScript zu parsen, kannst du den `Date` Konstruktor oder die `Date.parse()` Methode verwenden, wobei beide mit vielen Datum-String-Formaten umgehen können.

```javascript
// Einfache Nutzung des Date Konstruktors
let meinDatum = new Date('2023-04-01T12:00:00Z');
console.log(meinDatum.toString()); // "Sat Apr 01 2023 14:00:00 GMT+0200 (Mitteleuropäische Sommerzeit)"

// Nutzung von Date.parse()
let zeitstempel = Date.parse('01 April 2023 12:00 UTC');
let datumAusZeitstempel = new Date(zeitstempel);
console.log(datumAusZeitstempel.toString()); // "Sat Apr 01 2023 14:00:00 GMT+0200 (Mitteleuropäische Sommerzeit)"
```

Beachte, dass die Interpretation des Datums abhängig von der JavaScript-Engine des Browsers ist und unterschiedliche Ergebnisse liefern kann.

## Deep Dive:
Historisch gesehen war die Datumsanalyse in JavaScript fehleranfällig, weil verschiedene Browser unterschiedliche String-Formate unterschiedlich interpretierten. ECMAScript 5 führte einen standardisierten Date-String-Format (ISO 8601) ein, was das Ganze viel zuverlässiger machte.

Alternative Bibliotheken wie Moment.js, Date-fns oder Luxon bieten oft mehr Flexibilität und Funktionen, wie Zeitberechnungen, internationale Zeitformatierung und eine bessere Zeitzonenunterstützung, als die eingebauten JavaScript-Methoden.

Was die Implementierungsdetails betrifft, konvertiert `Date.parse()` einen Datumsstring in einen Unix-Zeitstempel (die Anzahl der Millisekunden seit dem 1. Januar 1970 UTC), und der `Date` Konstruktor kann diesen Zeitstempel dann verwenden, um ein entsprechendes Date-Objekt zu erstellen.

## See Also:
- MDN-Webdokumentation zu Date-Objekten: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- Überblick zur ISO 8601: https://de.wikipedia.org/wiki/ISO_8601
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Luxon: https://moment.github.io/luxon/
