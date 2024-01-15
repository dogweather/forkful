---
title:                "Das Berechnen von Daten in der Zukunft oder Vergangenheit"
html_title:           "TypeScript: Das Berechnen von Daten in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen von Daten in der Zukunft oder Vergangenheit"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Daten kann in vielen Fällen nützlich sein. Beispielsweise für die Planung von Terminen oder um die Zeitangaben von Ereignissen zu überprüfen.

## How To

Die Berechnung von Datumsangaben in TypeScript kann mithilfe der integrierten Date Klasse erfolgen. Hier ein Beispiel, um das Datum von heute um einen bestimmten Zeitraum zu ändern:

```TypeScript
let today: Date = new Date();
// Heutiges Datum: 30. Mai 2021
let futureDate: Date = today.setDate(today.getDate() + 7);
// Datum in 7 Tagen: 06. Juni 2021
```

Eine weitere Möglichkeit ist die Nutzung von Moment.js, einer JavaScript-Bibliothek zur Manipulation von Daten und Zeiten. Hier ein Beispiel für die Berechnung eines Datums in der Vergangenheit:

```TypeScript
import moment from 'moment';

let pastDate: Date = moment().subtract(1, 'week').toDate();
// Datum vor einer Woche: 23. Mai 2021
```

Um das Ergebnis übersichtlicher zu gestalten, kann das Datum auch in einem bestimmten Format ausgegeben werden. Hier ein Beispiel mit Hilfe von Moment.js:

```TypeScript
let futureDate: Date = moment().add(2, 'years').format('DD.MM.YYYY');
// Datum in 2 Jahren im Format: 30.05.2023
```

## Deep Dive

Die Datumsberechnung kann auch komplexer gestaltet werden, indem zum Beispiel auf Wochentage oder Monate geachtet wird. Die Date Klasse bietet dafür verschiedene Methoden wie `getDay()` oder `getMonth()` an. Zudem kann die Sprache und Zeitzone in Moment.js angepasst werden, um internationale Anforderungen zu erfüllen.

## Siehe auch

- [Moment.js Dokumentation](https://momentjs.com/docs/)
- [TypeScript Date Klasse](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)