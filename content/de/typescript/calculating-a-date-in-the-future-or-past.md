---
title:                "Eine zukünftige oder vergangene Datum berechnen"
html_title:           "TypeScript: Eine zukünftige oder vergangene Datum berechnen"
simple_title:         "Eine zukünftige oder vergangene Datum berechnen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zukünftige oder vergangene Daten zu berechnen, beinhaltet den Zeitunterschied zwischen dem aktuellen Datum und einem anderen Datum, das entweder in der Vergangenheit oder in der Zukunft liegt. Programmierer tun dies normalerweise, um Zeit-abhängige Funktionen wie Terminplanung, Erinnerungen und andere zeitbezogene Funktionen zu implementieren.

## So geht's:

In TypeScript können wir das native `Date`-Objekt verwenden, um zukünftige und vergangene Daten leicht zu errechnen. Hier sind ein paar Codebeispiele:

Zukünftige Daten berechnen:

```TypeScript
let zukunftsDatum = new Date();
zukunftsDatum.setDate(zukunftsDatum.getDate() + 5);
console.log(zukunftsDatum);
```

In diesem Fall erhöhen wir einfach die Anzahl der Tage um 5. Wenn Sie dies ausführen, würde dies ein Datum 5 Tage ab dem aktuellen Datum ausgeben.

Vergangene Daten berechnen:

```TypeScript
let vergangenesDatum = new Date();
vergangenesDatum.setDate(vergangenesDatum.getDate() - 10);
console.log(vergangenesDatum);
```

Ähnlich erhöhen wir die Anzahl der Tage einfach um -10, um ein Datum vor 10 Tagen zu erhalten. 

## Tiefergehende Informationen:

Historisch gesehen galt die Berechnung von Daten als eine komplexe Aufgabe, insbesondere aufgrund unterschiedlicher Kalender und Zeitzonen. Aber in modernen Programmiersprachen wie TypeScript haben eingebaute Date-Objekte diese Aufgabe stark vereinfacht.

Eine Alternative wäre die Verwendung von externen Bibliotheken wie Moment.js, die noch mehr Funktionen bieten als die in TypeScript eingebauten Methoden. Diese Bibliotheken können bei komplexeren Anforderungen von Vorteil sein.

Beim Berechnen von Daten in TypeScript sollte man beachten, dass das `Date`-Objekt Monate als 0-basierten Index behandelt. Sprich, Januar entspricht 0, Februar entspricht 1, und so weiter. 

## Weiterführende Links:

- [MDN JavaScript Date-Objekt](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Dokumentation](https://www.typescriptlang.org/docs/)
- [Moment.js Dokumentation](https://momentjs.com/)