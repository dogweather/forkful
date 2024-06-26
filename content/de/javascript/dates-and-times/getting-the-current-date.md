---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:57.084444-07:00
description: "Wie: In reinem JavaScript wird das `Date`-Objekt verwendet, um mit Datums-\
  \ und Zeitangaben zu arbeiten. Hier ist, wie Sie das aktuelle Datum und die\u2026"
lastmod: '2024-03-13T22:44:54.276554-06:00'
model: gpt-4-0125-preview
summary: In reinem JavaScript wird das `Date`-Objekt verwendet, um mit Datums- und
  Zeitangaben zu arbeiten.
title: Den aktuellen Datum abrufen
weight: 29
---

## Wie:
In reinem JavaScript wird das `Date`-Objekt verwendet, um mit Datums- und Zeitangaben zu arbeiten. Hier ist, wie Sie das aktuelle Datum und die Uhrzeit erhalten können:

```javascript
const currentDate = new Date();
console.log(currentDate); // Beispiel-Ausgabe: Fr Apr 14 2023 12:34:56 GMT+0100 (Britische Sommerzeit)
```

Um nur das Datum in einem benutzerfreundlicheren Format anzuzeigen, können Sie Methoden wie `toLocaleDateString()` verwenden:

```javascript
console.log(currentDate.toLocaleDateString()); // Beispiel-Ausgabe: 14.4.2023
```

Für mehr Kontrolle über das Format sind Drittanbieter-Bibliotheken wie *Moment.js* oder *date-fns* sehr beliebt, obwohl man sich bewusst sein sollte, dass Moment.js mittlerweile als Legacy-Projekt im Wartungsmodus gilt.

Mit *Moment.js*:

```javascript
const moment = require('moment'); // unter der Annahme von Node.js oder Verwendung eines Modulbündlers
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Beispiel-Ausgabe: 2023-04-14
```

Bei *date-fns*, das Modularisierung betont und es Ihnen erlaubt, nur das zu importieren, was Sie benötigen:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Beispiel-Ausgabe: 2023-04-14
```

Jeder Ansatz bietet unterschiedliche Grade an Bequemlichkeit und Flexibilität beim Arbeiten mit Daten in JavaScript, vom integrierten `Date`-Objekt bis hin zu anspruchsvolleren Formatierungs- und Manipulationsmöglichkeiten, die durch Bibliotheken verfügbar sind.
