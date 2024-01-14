---
title:                "TypeScript: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist in der Programmierung ein häufig angewandtes Verfahren. Es ermöglicht uns, festzustellen, ob ein Datum vor oder nach einem anderen Datum liegt, was besonders nützlich ist, wenn wir mit Zeitstempeln arbeiten. In diesem Blog-Beitrag werden wir uns ansehen, wie man in TypeScript zwei Daten vergleicht und welche Faktoren dabei beachtet werden sollten.

## Wie geht das?

Um zwei Daten in TypeScript zu vergleichen, können wir die `Date`-Klasse verwenden, die Teil des Standardbibliothekspakets ist. Diese Klasse bietet Methoden, um verschiedene Aspekte von Datum und Uhrzeit zu vergleichen. Schauen wir uns ein Beispiel an, um besser zu verstehen, wie dies funktioniert:

````TypeScript
const date1 = new Date("2021-01-01");
const date2 = new Date("2020-12-31");

console.log(date1 > date2); // Ausgabe: true
````

In diesem Beispiel erstellen wir zwei `Date`-Objekte und vergleichen sie mit dem größer-als-Operator (`>`). Da `date1` im Jahr 2021 liegt und damit nach  `date2` im Jahr 2020, wird `true` ausgegeben. Wir können auch andere Vergleichsoperatoren wie `>=`, `<` und `<=` verwenden, um weitere Vergleiche zwischen Daten durchzuführen.

## Tiefergehende Untersuchung

Beim Vergleichen von Daten gibt es einige Dinge zu beachten. Zunächst einmal müssen beide Daten im gleichen Format vorliegen, da sonst unerwartete Ergebnisse auftreten können. Zum Beispiel würde `new Date("01-01-2021")` ein anderes Ergebnis liefern als `new Date("2021-01-01")`.

Außerdem können auch die Zeitzone und die Werte der Stunden, Minuten und Sekunden eine Rolle spielen. Wenn wir also zwei Daten miteinander vergleichen, ist es wichtig, dass wir auch diese Faktoren berücksichtigen.

Eine weitere Sache, die wir beachten sollten, ist, dass `Date`-Objekte inkrementierbar sind. Das bedeutet, dass wir, wenn wir beispielsweise `date1.setDate(date1.getDate() + 1)` ausführen, das Datum um einen Tag erhöhen. Beim Vergleichen von Daten kann dies zu unerwarteten Ergebnissen führen, da sich das ursprüngliche `Date`-Objekt verändert und damit der Vergleich möglicherweise nicht mehr aussagekräftig ist.

## Siehe auch

- [Offizielle Dokumentation zu `Date` in TypeScript](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [Datum und Uhrzeit in TypeScript richtig manipulieren](https://dev.to/marknatividad/handling-dates-and-times-in-typescript-apps-24l7)
- [Moment.js - eine beliebte Bibliothek für die Arbeit mit Datum und Uhrzeit in JavaScript und TypeScript](https://momentjs.com/)