---
title:                "TypeScript: Das aktuelle Datum erhalten."
simple_title:         "Das aktuelle Datum erhalten."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man sich mit dem aktuellen Datum in TypeScript beschäftigen sollte. Zum Beispiel kann es hilfreich sein, um Zeitstempel in Programmen zu nutzen oder um die Dauer zwischen verschiedenen Datumswerten zu berechnen. In diesem Artikel werden wir uns damit beschäftigen, wie man in TypeScript das aktuelle Datum bekommen und nutzen kann.

## Wie 
Es gibt mehrere Möglichkeiten, in TypeScript das aktuelle Datum zu erhalten. Eine Möglichkeit ist die Verwendung der Date-Klasse. Hier ein Beispiel:

```TypeScript
const aktuellesDatum = new Date();
console.log(aktuellesDatum.getDate());
```

Das obige Beispiel erstellt eine neue Instanz der `Date`-Klasse und gibt den Tag des Monats des aktuellen Datums aus. Hier sind einige weitere Beispiele, wie man die Date-Klasse nutzen kann:

```TypeScript
const aktuellesDatum = new Date();
console.log(aktuellesDatum.getFullYear()); // Gibt das aktuelle Jahr aus
console.log(aktuellesDatum.getMonth()); // Gibt den aktuellen Monat aus (beginnend bei 0 für Januar)
console.log(aktuellesDatum.getHours()); // Gibt die aktuelle Stunde aus
console.log(aktuellesDatum.getMinutes()); // Gibt die aktuellen Minuten aus
console.log(aktuellesDatum.getSeconds()); // Gibt die aktuellen Sekunden aus
```

## Deep Dive
Die Date-Klasse bietet noch viele weitere Methoden, um mit Datumswerten zu arbeiten. Hier sind einige Beispiele:

- `getTime()`: Gibt die Anzahl der Millisekunden seit dem 1. Januar 1970 zurück
- `setFullYear()`: Setzt das Jahr des Datums auf den angegebenen Wert
- `setMonth()`: Setzt den Monat des Datums auf den angegebenen Wert
- `setDate()`: Setzt den Tag des Datums auf den angegebenen Wert

Es gibt auch die Möglichkeit, das Datum im ISO-8601-Format zu erhalten oder zu setzen. Dafür kann man die `toISOString()`-Methode nutzen.

## Siehe Auch
- [MDN Web Docs - Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Dokumentation - Date Objekt](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)