---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:15:03.080568-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

JavaScript ermöglicht es, das aktuelle Datum und die Uhrzeit zu ermitteln, was in vielen Anwendungen nützlich ist - etwa für Zeitstempel, Kalenderfunktionen oder einfach nur, um die lokale Uhrzeit eines Benutzers anzuzeigen.

## So geht's:

Um das aktuelle Datum zu bekommen, verwenden wir das `Date`-Objekt. Hier ist ein schneller Überblick:

```javascript
// Aktuelles Datum und Uhrzeit
const jetzt = new Date();
console.log(jetzt); // z.B. Wed Mar 31 2021 14:05:32 GMT+0200 (Mitteleuropäische Sommerzeit)

// Zu einzelnen Komponenten zugreifen
console.log(jetzt.getFullYear());    // z.B. 2021
console.log(jetzt.getMonth() + 1);   // z.B. 4 (Januar ist 0!)
console.log(jetzt.getDate());        // z.B. 31
console.log(jetzt.getHours());       // z.B. 14
console.log(jetzt.getMinutes());     // z.B. 5
console.log(jetzt.getSeconds());     // z.B. 32
```

## Deep Dive:

Das `Date`-Objekt in JavaScript existiert schon seit den Anfangstagen der Sprache. Es ist basiert auf der Zeit in Millisekunden seit dem UNIX-Epoch (1. Januar 1970 00:00:00 UTC). Es gibt verschiedene Methoden, um mit Zeit in JS zu arbeiten und spezifische Informationen zu erhalten.

Alternativen zu `new Date()` beinhalten Bibliotheken wie Moment.js, Date-fns oder day.js, die zusätzliche Funktionalitäten und einfacheres Parsing von Daten bieten, wobei native Funktionen in modernen Browsern meistens ausreichen.

Wichtig ist zu verstehen, dass `getMonth()` Monate von 0 bis 11 zählt, also muss man immer 1 hinzufügen. Zeitzonen können auch Probleme darstellen; `Date` arbeitet standardmäßig mit der lokalen Zeit des Benutzers. Für UTC-Zeit verwendet man entsprechende UTC-Methoden, z.B. `getUTCHours()`.

## Siehe auch:

- MDN Web Docs zu Date: [MDN Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Zeitbibliotheken für kompliziertere Aufgaben: [Moment.js](https://momentjs.com/), [Date-fns](https://date-fns.org/), [Day.js](https://day.js.org/)
- Unterschiede zwischen UTC und lokaler Zeit: [UTC vs Local Time](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date#UTC_and_local_time)
