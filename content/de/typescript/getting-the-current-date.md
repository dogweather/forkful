---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# TypeScript-Programmierung: Abrufen des aktuellen Datums

## Was & Warum?
Abrufen des aktuellen Datums ist ein gängiger Prozess in der Programmierung, bei dem wir die Information über Datum und Uhrzeit sammeln. Wir tun es, um Kontext bereitzustellen, Protokolle zu generieren oder zu verfolgen, wann bestimmte Ereignisse in unseren Anwendungen auftreten.

## Wie geht das:

Unten finden Sie ein kurzes Beispiel, wie Sie das aktuelle Datum in TypeScript abrufen können:

```TypeScript
let currentDate = new Date(); 
console.log(currentDate);
```

Die Ausgabe wird wie folgt sein:

```TypeScript
2022-03-23T16:38:06.373Z
```

Hierbei gibt `new Date()` das aktuelle Datum und die aktuelle Uhrzeit zurück.

## Deep Dive

Historisch gesehen wurde in JavaScript, und folglich in TypeScript, stets die Date()-Funktion zum Abrufen des aktuellen Datums verwendet. Alternative Methoden sind eher selten, da diese Funktion einfach zu verwenden und in den meisten Szenarien ausreichend ist.

Allerdings ist es wichtig zu beachten, dass `new Date()` das Datum und die Uhrzeit entsprechend der Systemzeitzone zurückgibt. Für eine universelle Zeitmarkierung könnte man `new Date().toISOString()` verwenden.

```TypeScript
let currentDate = new Date(); 
let isoDate = currentDate.toISOString();
console.log(isoDate);
```

## Siehe auch:

- [MDN Web Docs: Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript: Working with Dates](https://www.typescriptlang.org/docs/handbook/utility-types.html#datetimestring)
- [Stack Overflow: How to get current date in typescript?](https://stackoverflow.com/questions/51464380/how-to-get-current-date-in-typescript)

Denken Sie daran, dass die beste Praxis immer von Ihrem spezifischen Anwendungsfall abhängt. Für den tiefen Einblick in die Datum- und Zeitverwaltung in TypeScript steht eine Fülle von Ressourcen zur Verfügung.