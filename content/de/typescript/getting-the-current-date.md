---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:16:48.785213-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist der Prozess, bei dem dein Programm das heutige Datum und die Uhrzeit ermittelt. Entwickler brauchen das für Funktionen wie Datumsstempel, Zeitverfolgung oder Gültigkeitsprüfungen.

## How to:
Um das aktuelle Datum in TypeScript zu holen, nutzt man das `Date` Objekt so:

```typescript
// Das aktuelle Datum und Uhrzeit holen
const jetzt = new Date();

// Datum in einem String-Format ausgeben
console.log(jetzt.toString());

// Nur das Datum ohne Zeit ausgeben
console.log(jetzt.toDateString());

// Datum und Zeit in ISO-Format (YYYY-MM-DDTHH:mm:ss.sssZ)
console.log(jetzt.toISOString());
```

Beispielausgabe:

```
Fri Apr 14 2023 16:20:13 GMT+0200 (Central European Summer Time)
Fri Apr 14 2023
2023-04-14T14:20:13.000Z
```

## Deep Dive
Das `Date` Objekt in JavaScript (und somit auch in TypeScript) existiert seit den Anfängen der Sprache. Alternativen wie `moment.js` oder `date-fns` bieten mehr Funktionen und Flexibilität, sind aber für einfache Anwendungen meist Overkill. Beim Arbeiten mit Zeitzonen und Lokalisierung können diese Bibliotheken jedoch sehr hilfreich sein. TypeScript verbessert das Arbeiten mit `Date` durch Typisierung und verbessert so die Lesbarkeit und Debugging.

## See Also
- MDN Web Docs zu `Date`: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- `moment.js`: https://momentjs.com/
- `date-fns`: https://date-fns.org/
