---
title:    "TypeScript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum 

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann nützlich sein, um beispielsweise in einem Kalender die Termine für die nächsten Wochen oder Monate zu berechnen.

# Wie 

Die Berechnung von Daten in TypeScript ist relativ einfach. Zunächst müssen wir das `Date`-Objekt importieren. Dann können wir das aktuelle Datum durch die Verwendung von `new Date()` erhalten. Wir können auch ein spezifisches Datum angeben, indem wir die entsprechenden Parameter (Jahr, Monat, Tag) in die Klammern von `new Date()` einfügen. Anschließend können wir mithilfe von verschiedenen Methoden und Operatoren das gewünschte Datum in der Zukunft oder Vergangenheit berechnen.

Ein Beispiel, um das Datum von 7 Tagen in der Zukunft zu berechnen:

```TypeScript
import { Date } from 'ts-lib';
let currentDate = new Date();
let futureDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate() + 7);
console.log(futureDate);
```

Die Ausgabe sieht folgendermaßen aus:

```
Sun Nov 21 2021 00:00:00 GMT+0200 (Eastern European Standard Time)
```

# Deep Dive 

Es gibt viele verschiedene Methoden und Operatoren, die beim Berechnen von Daten in der Zukunft oder Vergangenheit verwendet werden können. Beispielsweise können wir mithilfe des `set`-Operators das Datum direkt ändern oder mithilfe von `getTime()` eine Zeitstempel-basierte Berechnung durchführen. Es ist auch möglich, die verschiedenen Methoden und Operatoren zu kombinieren, um komplexe Berechnungen durchzuführen.

# Siehe auch 

- [Offizielle TypeScript-Dokumentation zu Date-Objekten](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Tutorial: Das JavaScript Date-Objekt](https://www.w3schools.com/js/js_dates.asp)
- [Beispiel für eine Datumsberechnung in TypeScript](https://dzone.com/articles/date-math-how-to-manipulate-date-strings-in-types)