---
date: 2024-01-20 17:37:24.682317-07:00
description: "Datum in String umwandeln bedeutet, ein `Date`-Objekt in eine lesbare\
  \ Zeichenkette zu verwandeln. Programmierer machen das, um Daten benutzerfreundlich\u2026"
lastmod: '2024-03-11T00:14:28.182704-06:00'
model: gpt-4-1106-preview
summary: "Datum in String umwandeln bedeutet, ein `Date`-Objekt in eine lesbare Zeichenkette\
  \ zu verwandeln. Programmierer machen das, um Daten benutzerfreundlich\u2026"
title: Datum in einen String umwandeln
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Datum in String umwandeln bedeutet, ein `Date`-Objekt in eine lesbare Zeichenkette zu verwandeln. Programmierer machen das, um Daten benutzerfreundlich anzuzeigen oder zu speichern.

## How to (Wie geht's)
```javascript
const heute = new Date(); // Aktuelles Datum und Uhrzeit

// Einfache Umwandlung in einen String
const datumAlsString = heute.toString();
console.log(datumAlsString); // z.B. "Mon Apr 03 2023 14:26:33 GMT+0200 (Central European Summer Time)"

// In ein lokales Format umwandeln
const lokalDatumString = heute.toLocaleDateString('de-DE');
console.log(lokalDatumString); // "03.04.2023"

// Uhrzeit dazu
const lokalZeitString = heute.toLocaleTimeString('de-DE');
console.log(lokalZeitString); // "14:26:33"

// Datum und Uhrzeit zusammen in lokalem Format
const lokalDatumZeitString = heute.toLocaleString('de-DE');
console.log(lokalDatumZeitString); // "03.04.2023, 14:26:33"

// ISO-8601-Format
const isoString = heute.toISOString();
console.log(isoString); // "2023-04-03T12:26:33.389Z"
```

## Deep Dive (Tieftauchen)
Ursprünglich verwendeten Programmierer das `Date`-Objekt im ECMAScript (JavaScript-Standard) für Zeitangaben. Im Laufe der Zeit erkannte man den Bedarf für verschiedene Darstellungsformen, wie das ISO-Format oder lokalisierte Strings.

Zu den Alternativen gehört `Date.prototype.toDateString()` für nur das Datum und `Date.prototype.toTimeString()` für nur die Uhrzeit.

Über die Implementierungsdetails: JavaScript-Engines nutzen das Internationalization API für `toLocaleString`-Methoden. Sie passen die Ausgabe an das gewählte Locale an, beispielsweise `de-DE` für Deutsch (Deutschland).

## See Also (Siehe auch)
- MDN Web Docs zu `Date`: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- ISO 8601 Format Info: https://www.iso.org/iso-8601-date-and-time-format.html
- ECMAScript 2022 Sprachspezifikation: http://www.ecma-international.org/publications/standards/Ecma-262.htm
