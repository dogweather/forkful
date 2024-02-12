---
title:                "Datum in einen String umwandeln"
aliases:
- /de/typescript/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:46.458283-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Datums in einen String ist der Prozess, bei dem ein Datum als lesbare Zeichenkette dargestellt wird. Programmierer nutzen dies, um Daten benutzerfreundlich anzuzeigen oder in einem bestimmten Format zu speichern.

## Vorgehensweise:
```TypeScript
const date: Date = new Date("2023-03-15T12:00:00Z");

// Beispiel 1: Umwandlung in lokales Datums- und Zeitformat
const dateStr1: string = date.toLocaleString('de-DE');
console.log(dateStr1); // "15.03.2023, 13:00:00"

// Beispiel 2: Umwandlung nur des Datums in lokales Format
const dateStr2: string = date.toLocaleDateString('de-DE');
console.log(dateStr2); // "15.03.2023"

// Beispiel 3: Manuelles Formatieren (Jahr-Monat-Tag)
const manualDateStr: string = `${date.getFullYear()}-${(date.getMonth() + 1).toString().padStart(2, '0')}-${date.getDate().toString().padStart(2, '0')}`;
console.log(manualDateStr); // "2023-03-15"
```

## Vertiefung:
Früher verwendete man in JavaScript oft externe Bibliotheken wie `moment.js` für Datumsoperationen. Heute bietet TypeScript, das Superset von JavaScript, vielfältige native Möglichkeiten. Die Wahl der Methode hängt vom Anwendungsfall ab: `.toLocaleString()` und `.toLocaleDateString()` sind praktisch für internationalisierte Anwendungen, da sie automatisch das Datum entsprechend der übergebenen `locale` – hier `'de-DE'` für Deutsch (Deutschland) – formatieren. Für komplexe Formatierungen oder wenn man ohne die Internationale Datums- und Zeitformate auskommen möchte, können getrennte Getter-Methoden genutzt werden.

Bei manueller Formatierung besteht auch mehr Kontrolle, doch es ist fehleranfälliger und erfordert mehr Code. Alternativen wie `date-fns` oder `Day.js` bieten eine moderne, modulare Herangehensweise, die oft besser zur Baumstruktur moderner Webanwendungen passt.

## Siehe auch:
- MDN Web Docs zu `Date` Objekt: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- TypeScript-Handbuch: https://www.typescriptlang.org/docs/handbook/intro.html
- `date-fns` Bibliothek: https://date-fns.org/
- `Day.js` Bibliothek: https://day.js.org/
