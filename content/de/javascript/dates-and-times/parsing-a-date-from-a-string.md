---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:20.175523-07:00
description: "Wie geht das? JavaScript bietet nativ die Methode `Date.parse()` und\
  \ den `Date`-Konstruktor zum Parsen von Datumsstrings an. Jedoch haben diese Ans\xE4\
  tze\u2026"
lastmod: '2024-03-13T22:44:54.275490-06:00'
model: gpt-4-0125-preview
summary: JavaScript bietet nativ die Methode `Date.parse()` und den `Date`-Konstruktor
  zum Parsen von Datumsstrings an.
title: Einen Datum aus einem String analysieren
weight: 30
---

## Wie geht das?
JavaScript bietet nativ die Methode `Date.parse()` und den `Date`-Konstruktor zum Parsen von Datumsstrings an. Jedoch haben diese Ansätze ihre Grenzen und Inkonsistenzen über verschiedene Browser hinweg, besonders bei nicht-standardisierten Datumsformaten. Um diese Probleme zu adressieren, sind Third-Party-Bibliotheken wie `Moment.js` und `date-fns` aufgrund ihrer Robustheit und Benutzerfreundlichkeit beliebt.

### Verwendung von nativem JavaScript:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // Ausgabe: Sun Apr 30 2023 14:55:00 GMT+0000 (Koordinierte Weltzeit)
```

### Verwendung von Moment.js:
Zuerst installiere Moment.js über npm oder füge es in dein Projekt ein. Dann:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // Ausgabe: Sun Apr 30 2023 14:55:00 GMT+0000
```

### Verwendung von date-fns:
Nachdem du `date-fns` zu deinem Projekt hinzugefügt hast, parse einen Datumsstring wie folgt:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // Ausgabe: 2023-04-30T14:55:00.000Z
```

Sowohl `Moment.js` als auch `date-fns` bieten umfassendere Parsing-Fähigkeiten, einschließlich der Handhabung verschiedener Formate und Lokalisierungen, was sie für komplexe Anwendungen bevorzugt macht.
