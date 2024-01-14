---
title:    "TypeScript: Vergleich von zwei Daten"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein häufiges Szenario in der Programmierung. Es kann hilfreich sein, um festzustellen, ob eine bestimmte Aktion ausgeführt werden soll oder um die Reihenfolge von Ereignissen zu bestimmen. In diesem Blogbeitrag werden wir uns ansehen, wie man mit TypeScript zwei Daten vergleichen kann.

## So geht's

Um zwei Daten in TypeScript zu vergleichen, müssen wir zunächst sicherstellen, dass diese in einem geeigneten Format vorliegen. Zum Beispiel können wir sie als `Date` Objekte deklarieren oder sie aus einem String parsen. Sobald wir eine `Date` Variable haben, können wir die vergleichenden Operatoren verwenden, um sie miteinander zu vergleichen. Schauen wir uns ein Beispiel an:

```TypeScript
// Deklaration von zwei Datum Variablen
let date1: Date = new Date("2021-01-01");
let date2: Date = new Date("2021-02-01");

// Vergleich der Daten
if (date1 < date2) {
    console.log("date1 ist früher als date2");
} else if (date1 > date2) {
    console.log("date1 ist später als date2");
} else {
    console.log("date1 und date2 sind gleich");
}

// Ausgabe: date1 ist früher als date2
```

In diesem Beispiel haben wir zwei `Date` Objekte erstellt und diese unter Verwendung des `<` und `>` Operators verglichen. Je nach Ergebnis geben wir eine entsprechende Nachricht aus.

Es ist auch möglich, die `getTime()` Methode zu verwenden, um die Millisekunden seit dem 1. Januar 1970 (auch als Unix-Zeit bekannt) zu erhalten und diese miteinander zu vergleichen. Hier ist ein weiteres Beispiel:

```TypeScript
// Deklaration von zwei Datum Variablen
let date1: Date = new Date("2021-01-01");
let date2: Date = new Date("2021-02-01");

// Vergleich der Daten
if (date1.getTime() < date2.getTime()) {
    console.log("date1 ist früher als date2");
} else if (date1.getTime() > date2.getTime()) {
    console.log("date1 ist später als date2");
} else {
    console.log("date1 und date2 sind gleich");
}

// Ausgabe: date1 ist früher als date2
```

## Tiefergehende Informationen

Es gibt noch viele weitere Möglichkeiten, um Daten in TypeScript zu vergleichen, wie zum Beispiel das Verwenden von `getTimezoneOffset()` oder `getFullYear()` Methoden. Es ist wichtig, das geeignete Vergleichsverfahren basierend auf den Anforderungen der Anwendung auszuwählen. Außerdem müssen möglicherweise auch Zeitzone und andere Faktoren berücksichtigt werden.

## Siehe auch

- [Das Date Objekt in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#the-date-object)
- [Vergleichsoperatoren in TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#comparison-operator-behavior)