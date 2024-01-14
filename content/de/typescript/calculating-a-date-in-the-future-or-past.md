---
title:    "TypeScript: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann in vielen Situationen nützlich sein. Zum Beispiel, um zu überprüfen, welcher Tag der Woche ein bestimmtes Datum fallen wird, oder um herauszufinden, wann ein wichtiger Termin in Bezug auf andere Termine liegt. Mit TypeScript können wir diese Berechnungen effizient und genau durchführen.

## Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zunächst das Datum auswählen, von dem aus wir berechnen wollen. Dann müssen wir die Anzahl der Tage, Wochen, Monate oder Jahre angeben, die wir hinzufügen oder subtrahieren möchten. Anhand dieser Informationen können wir ein neues Datum erstellen.

```TypeScript
const startDatum = new Date(2021, 4, 25); // 25. Mai 2021
const zukunftsDatum = new Date(startDatum.getFullYear() + 1, startDatum.getMonth() + 3, startDatum.getDate()); // 25. August 2022
```

In diesem Beispiel haben wir das Datum 25. Mai 2021 ausgewählt und dann ein neues Datum erstellt, das ein Jahr und drei Monate später liegt. Wir können auch negative Werte verwenden, um ein Datum in der Vergangenheit zu berechnen.

```TypeScript
const startDatum = new Date(2021, 7, 12); // 12. August 2021
const vergangenheitsDatum = new Date(startDatum.getFullYear() - 2, startDatum.getMonth() - 8, startDatum.getDate()); // 12. Dezember 2018
```

In diesem Beispiel haben wir das Datum 12. August 2021 ausgewählt und dann ein neues Datum erstellt, das zwei Jahre und acht Monate zuvor liegt.

## Tiefer Einblick

Bei der Berechnung von Datumswerten ist es wichtig zu beachten, dass die Monate in TypeScript bei 0 beginnen. Das heißt, der Januar ist Monat 0, der Februar ist Monat 1 usw. Deshalb haben wir in den obigen Beispielen die Monate um eins erhöht oder verringert, um das korrekte zukünftige oder vergangene Datum zu erhalten. 

Außerdem können wir die berechneten Datumsobjekte auch in bestimmten Formaten ausgeben, indem wir die Methoden `getDate()`, `getMonth()` und `getFullYear()` verwenden. Zum Beispiel:

```TypeScript
const zukunftsDatum = new Date(2021, 0, 1);
console.log(`Das berechnete Datum ist: ${zukunftsDatum.getDate()}.${zukunftsDatum.getMonth() + 1}.${zukunftsDatum.getFullYear()}`); // Das berechnete Datum ist: 1.1.2022
```

## Siehe auch

- Dokumentation zu JavaScript-Datumsobjekten: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Wie man mit TypeScript arbeitet: https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html
- Ein Praxisbeispiel für die Verwendung von berechneten Datumswerten: https://dev.to/chrisachard/adventure-game-in-6502-assembly-ari