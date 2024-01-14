---
title:                "TypeScript: Die aktuelle Datum abrufen"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums kann für Programmiererinnen und Programmierer sehr hilfreich sein, um verschiedene Aufgaben und Funktionen in ihrem Code auszuführen. Es kann verwendet werden, um zeitgesteuerte Aktionen durchzuführen, Daten zu organisieren oder um den Nutzer mit dem aktuellen Datum zu begrüßen.

## Wie Geht's

Das Abrufen des aktuellen Datums in TypeScript ist relativ einfach. Verwenden Sie einfach die integrierte JavaScript-Methode `new Date()`, um ein Date-Objekt zu erstellen. Sie können dann verschiedene Methoden anwenden, um das Datum in verschiedenen Formaten darzustellen.

### Beispiel:

```TypeScript
let currentDate: Date = new Date();
console.log(currentDate); // Ausgabe: Tue Sep 08 2020 09:30:00 GMT+0200 (Mitteleuropäische Sommerzeit)

let day: number = currentDate.getDate();
let month: string = currentDate.getMonth() + 1;
let year: number = currentDate.getFullYear();
console.log(`${day}.${month}.${year}`); // Ausgabe: 08.09.2020
```

Sie können auch die `toLocaleString()`-Methode verwenden, um das Datum in verschiedenen Länder- und Sprachformaten darzustellen.

### Beispiel:

```TypeScript
console.log(currentDate.toLocaleString('de-DE')); // Ausgabe: 8.9.2020, 09:30:00
console.log(currentDate.toLocaleString('en-US')); // Ausgabe: 9/8/2020, 9:30:00 AM
```

## Tiefer Tauchen

Der `new Date()`-Konstruktor ermöglicht es uns, nicht nur das aktuelle Datum zu erstellen, sondern auch ein spezifisches Datum in der Vergangenheit oder Zukunft. Wir können auch Zeitangaben hinzufügen, um das Datum und die Uhrzeit zu steuern.

### Beispiel:

```TypeScript
let date: Date = new Date(2020, 0, 1, 5, 30, 0); // 1. Januar 2020 um 5:30 Uhr
console.log(date.toLocaleString('de-DE')); // Ausgabe: 1.1.2020, 05:30:00
```

Sie können auch verschiedene Methoden anwenden, um das Datum zu manipulieren, z. B. `setDate()` oder `setFullYear()`. Schauen Sie sich die offizielle [Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date) für weitere Details und Beispiele an.

## Siehe Auch

- [Date-Objekt, MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript: Official Website](https://www.typescriptlang.org/)
- [JavaScript Dates, W3Schools](https://www.w3schools.com/js/js_dates.asp)