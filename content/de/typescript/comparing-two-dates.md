---
title:                "TypeScript: Zwei Datumsangaben vergleichen"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist in der Programmierung ein häufiges Szenario. Es ermöglicht uns, zu überprüfen, ob ein Datum vor oder nach einem anderen liegt, was wichtig sein kann, wenn wir beispielsweise bestimmte Aktionen abhängig von bestimmten Zeiträumen ausführen möchten. In diesem Blog-Beitrag werden wir uns ansehen, wie man mit TypeScript zwei Daten vergleichen kann.

## How To

Um zwei Daten in TypeScript zu vergleichen, müssen wir zunächst sicherstellen, dass wir beide Daten in einem geeigneten Format haben. Dies kann ein `Date`-Objekt oder ein `string` sein, das ein Datum repräsentiert. Als nächstes können wir den Vergleichsoperator `>` oder `<` verwenden, um zu überprüfen, ob ein Datum vor oder nach dem anderen liegt. Schauen wir uns ein Beispiel an:

```TypeScript
const firstDate = new Date('2021-10-10');
const secondDate = new Date('2021-10-20');

if (firstDate > secondDate) {
  console.log('Das erste Datum liegt nach dem zweiten Datum.');
} else if (firstDate < secondDate) {
  console.log('Das erste Datum liegt vor dem zweiten Datum.');
} else {
  console.log('Die beiden Daten sind identisch.');
}
```

Dieses Beispiel erstellt zwei `Date`-Objekte und vergleicht sie mithilfe des `>` und `<` Operators. Je nachdem, welches Datum vor oder nach dem anderen liegt, wird eine entsprechende Nachricht ausgegeben. Probieren wir das Ganze mit dem TypeScript Compiler aus und schauen wir uns die Ausgabe an:

```shell
tsc date-comparison.ts && node date-comparison.js

Das erste Datum liegt vor dem zweiten Datum.
```

Wenn wir jedoch beide Daten gleich setzen, wird die Ausgabe folgendermaßen aussehen:

```shell
tsc date-comparison.ts && node date-comparison.js

Die beiden Daten sind identisch.
```

## Deep Dive

Möglicherweise möchtest du auch wissen, ob zwei Daten den gleichen Tag oder den gleichen Monat haben. Dafür können wir den `getDate()` und den `getMonth()` Methode des `Date`-Objekts verwenden. Schauen wir uns wieder ein Beispiel an:

```TypeScript
const firstDate = new Date('2021-10-10');
const secondDate = new Date('2021-10-20');

if (firstDate.getMonth() === secondDate.getMonth()) {
  console.log('Die beiden Daten haben den gleichen Monat.');
}

if (firstDate.getDate() === secondDate.getDate()) {
  console.log('Die beiden Daten haben den gleichen Tag.');
}
```

Hier verwenden wir die `getDate()` und `getMonth()` Methode, um den Monat und den Tag jedes Datums abzurufen und sie miteinander zu vergleichen. Wenn die Bedingungen erfüllt sind, wird eine entsprechende Nachricht ausgegeben.

## Siehe auch

Für weitere Informationen zum Umgang mit Daten in TypeScript, schauen Sie sich gerne die folgenden Links an:

- [Offizielle TypeScript Dokumentation](https://www.typescriptlang.org/docs/)
- [Vergleichen von zwei Daten in JavaScript](https://www.w3schools.com/js/js_date_methods.asp)
- [Überblick über vergleichbare Operatoren in TypeScript](https://www.typescriptlang.org/docs/handbook/advanced-types.html#comparable-types)