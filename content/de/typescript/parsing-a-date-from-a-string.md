---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:38:55.983285-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, ein Datum in einem textbasierten Format zu nehmen und es in ein Datumsobjekt umzuwandeln, das in TypeScript verwendet werden kann. Programmierer machen das, um Datumsangaben aus Benutzereingaben oder Datenquellen einfach und sicher zu handhaben.

## How to:
Hier ist ein grundlegendes Beispiel, wie man ein Datum in TypeScript aus einem String parst:

```TypeScript
// Vorherige Einrichtung
let dateString: string = "2023-04-05";

// Datum aus einem String parsen
let parsedDate: Date = new Date(dateString);

console.log(parsedDate);
```

Ausgabe:
```
2023-04-05T00:00:00.000Z
```
Beachte, dass die Zeitzone auf UTC gesetzt ist und anzeigt, dass keine Zeitinformation vorhanden ist.

## Deep Dive
Datum zu parsen ist nicht neu - schon lange vor TypeScript gab es in vielen Programmiersprachen Funktionen dazu. Doch Vorsicht: Dates in JavaScript und TypeScript können tückisch sein, weil verschiedene Browser und Node.js unterschiedlich mit Zeitangaben und Zeitzonen umgehen können.

Alternativen zum eingebauten `Date`-Objekt sind Bibliotheken wie `Moment.js`, `date-fns` oder `Day.js`. Diese bieten mehr Flexibilität und Konsistenz, insbesondere bei der Formatierung und beim Umgang mit Zeitzonen.

Implementation: TypeScript selbst fügt beim Datum-Parsing keine neuen Funktionen hinzu, es nutzt das `Date`-Objekt von JavaScript. Daher ist es wichtig, die Besonderheiten von JavaScript-Dates zu verstehen, etwa dass Monate bei 0 beginnen (0 für Januar, 1 für Februar, usw.).

## See Also
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns](https://date-fns.org/)
- [Day.js](https://day.js.org/)
- [Moment.js](https://momentjs.com/)

Diese Dokumentationen und Bibliotheken bieten weitere Einblicke und Hilfsmittel für den Umgang mit Datum und Zeit in deinen TypeScript-Projekten.