---
title:                "Vergleich zweier Daten"
html_title:           "TypeScript: Vergleich zweier Daten"
simple_title:         "Vergleich zweier Daten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

Was & Warum?

Das Vergleichen von zwei Datumsangaben ist ein gängiges Problem für Programmierer. Es ermöglicht die Überprüfung, ob ein Datum früher oder später als ein anderes ist. Dadurch kann zum Beispiel die korrekte Reihenfolge von Ereignissen in einer Anwendung gewährleistet werden.

Wie:

Das Vergleichen von zwei Datumsangaben in TypeScript ist relativ einfach. Zunächst müssen wir die beiden Datumsangaben als Objekte vom Typ 'Date' erstellen. Dann können wir den Vergleichsoperator '>' oder '<' verwenden, um zu überprüfen, ob das erste Datum früher oder später als das zweite ist. Hier ist ein Beispiel:

```TypeScript
let date1: Date = new Date(2021, 11, 31);
let date2: Date = new Date(2022, 0, 1);

if (date1 > date2) {
    console.log('Date 1 is later than Date 2');
} else {
    console.log('Date 1 is earlier than Date 2');
}
```

Der Output wäre in diesem Fall: 'Date 1 is earlier than Date 2'.

Deep Dive:

Das Vergleichen von Datumsangaben hat eine lange Geschichte und geht zurück bis ins antike Ägypten. Dort wurde der Kalender von den Pharaonen verwendet, um das Datum des jährlichen Nil-Hochwassers vorherzusagen. Heutzutage gibt es viele verschiedene Arten, wie Datumsangaben in Computerprogrammen dargestellt werden können, zum Beispiel die Unix-Zeit oder ISO-Datumsformate.

Es gibt auch alternative Möglichkeiten, um zwei Datumsangaben zu vergleichen. Eine davon ist die Verwendung der 'getTime()' Funktion, die die Millisekunden seit dem 1. Januar 1970 zurückgibt. Durch die Verwendung dieser Funktion können wir zwei Datumsangaben direkt miteinander vergleichen, ohne die Verwendung von Vergleichsoperatoren.

See Also:

- [TypeScript Documentation on Date Objects](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)
- [MDN Web Docs on Date Objects](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [History of Calendars and the Gregorian Calendar](https://www.history.com/news/6-calendars-better-than-yours)