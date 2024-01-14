---
title:                "TypeScript: Eine zukünftige oder vergangene Datum berechnen"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann besonders in der Softwareentwicklung sehr hilfreich sein, um beispielsweise Termine oder Fristen zu überprüfen oder zu planen. Mit TypeScript, einer typisierten Erweiterung von JavaScript, ist es möglich, diese Berechnungen genau und effizient durchzuführen.

## So geht's

Die grundlegende Methode, um ein Datum in TypeScript zu berechnen, ist `new Date()`. Dieser Konstruktor erzeugt ein neues Datum-Objekt, welches das aktuelle Datum und die aktuelle Uhrzeit enthält.

Um nun ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir dem Konstruktor ein Argument übergeben, welches die Zahl der Millisekunden angibt, die zu dem aktuellen Datum hinzugefügt oder abgezogen werden sollen.

**Beispiel 1: Berechnung eines Datums in der Zukunft:**

```TypeScript
let heutigesDatum: Date = new Date();  
let zukuenftigesDatum: Date = new Date(heutigesDatum.getTime() + 7 * 24 * 60 * 60 * 1000); // 7 Tage in Millisekunden
console.log(zukuenftigesDatum); // Ausgabe: 2020-10-24T22:00:00.000Z
```

**Beispiel 2: Berechnung eines Datums in der Vergangenheit:**

```TypeScript
let heutigesDatum: Date = new Date();  
let vergangenesDatum: Date = new Date(heutigesDatum.getTime() - 10 * 24 * 60 * 60 * 1000); // 10 Tage in Millisekunden
console.log(vergangenesDatum); // Ausgabe: 2020-10-11T22:00:00.000Z
```

Beachten Sie, dass die Zeitzonen bei diesen Berechnungen berücksichtigt werden. Die Ausgabe kann je nach Standort variieren.

## Deep Dive

Es gibt noch viele weitere Methoden und Möglichkeiten, um Datumsangaben in TypeScript zu berechnen. Zum Beispiel können wir auch das aktuelle Datum mithilfe der `set`-Methoden anpassen oder ein Datum aus einem String erstellen.

Ein weiterer wichtiger Punkt ist die Handhabung von Schaltjahren. Das Datum-Schema ist nicht für jedes Jahr gleich, daher sollten wir bei Berechnungen immer auf mögliche Schaltjahre achten. Eine detaillierte Erklärung dazu würde jedoch den Rahmen dieses Blog-Beitrags sprengen.

Für weiterführende Informationen empfehle ich die offizielle Dokumentation von TypeScript, die zahlreiche Beispiele und detaillierte Erklärungen zu den verschiedenen Methoden bietet.

## Siehe auch

- [TypeScript Dokumentation](https://www.typescriptlang.org/docs/home.html)
- [Typescript - Das JavaScript für Enterprise-Systeme](https://entwickler.de/online/javascript/typescript-javascript-enterprise-577806255.html)
- [10 Gründe, warum man TypeScript verwenden sollte](https://blog.mariusschulz.com/2017/02/02/10-reasons-why-you-should-be-using-typescript)