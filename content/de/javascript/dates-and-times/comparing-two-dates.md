---
date: 2024-01-20 17:33:30.887383-07:00
description: "How to: In JavaScript wurden Datumsvergleiche schon immer durchgef\xFC\
  hrt, indem man `Date` Objekte umwandelt und deren Zeitstempel vergleicht. Das `Date`\u2026"
lastmod: '2024-04-05T21:53:56.166739-06:00'
model: gpt-4-1106-preview
summary: "In JavaScript wurden Datumsvergleiche schon immer durchgef\xFChrt, indem\
  \ man `Date` Objekte umwandelt und deren Zeitstempel vergleicht."
title: Vergleich von zwei Daten
weight: 27
---

## How to:
```javascript
const date1 = new Date('2023-03-30');
const date2 = new Date('2023-04-01');

// Überprüfung, ob date1 vor date2 liegt
console.log(date1 < date2);  // true

// Überprüfung auf Gleichheit (mithilfe von getTime())
console.log(date1.getTime() === date2.getTime());  // false

// Überprüfung, ob date1 nach date2 liegt
console.log(date1 > date2);  // false
```

## Deep Dive
In JavaScript wurden Datumsvergleiche schon immer durchgeführt, indem man `Date` Objekte umwandelt und deren Zeitstempel vergleicht. Das `Date` Objekt gibt dir dabei Millisekunden seit dem 1. Januar 1970. Achtung: zwei Date-Objekte direkt zu vergleichen (`date1 === date2`) geht nicht, weil es sich um unterschiedliche Objektreferenzen handelt. Für exakte Gleichheitsvergleiche muss `.getTime()` genutzt werden; diese Funktion konvertiert das Datum in eine Zahl, die den Zeitstempel repräsentiert.

Alternativen: Neben dem nativen `Date` Objekt gibt es Bibliotheken wie `moment.js` oder `date-fns`, die mehr Funktionalität bieten. Sie können hilfreich sein, wenn es um komplexere Datumsoperationen oder Formatierungen geht.

## See Also
- MDN Web Docs zum `Date` Objekt: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Info zu `date-fns`, einer modernen JavaScript-Bibliothek: https://date-fns.org/
- Moment.js, eine populäre Bibliothek für Datumsoperationen: https://momentjs.com/
