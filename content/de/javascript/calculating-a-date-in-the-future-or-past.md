---
title:                "Javascript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann in der Programmierung sehr nützlich sein, um Abläufe oder Ereignisse zu planen und zu organisieren. Es ermöglicht auch die interaktive Gestaltung von Benutzeroberflächen, beispielsweise bei der Erstellung von Kalender- oder Terminplanungsanwendungen.

## Wie geht man vor

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es in Javascript verschiedene Methoden und Funktionen, die verwendet werden können. In diesem Blogpost werden wir uns auf das Manipulieren von Datumsobjekten mit der integrierten Javascript Date-Klasse konzentrieren.

Zunächst müssen wir ein Date-Objekt erstellen, das das aktuelle Datum und die aktuelle Uhrzeit enthält. Dies kann mit dem Befehl new Date() erreicht werden. Wir können auch ein bestimmtes Datum und eine Uhrzeit angeben, indem wir die entsprechenden Parameter in diese Funktion einbeziehen, z.B. new Date(2020, 3, 7), um den 7. April 2020 zu erstellen. Die Monate werden hierbei jedoch von 0 bis 11 gezählt, also entspricht 3 dem Monat April.

Um nun ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Funktionen setFullYear(), setMonth() und setDate() verwenden, um das Jahr, den Monat und den Tag des Datums anzupassen. Dazu müssen wir jedoch zunächst das ursprüngliche Date-Objekt kopieren, da die set-Methoden die ursprünglichen Daten überschreiben würden. Dies kann z.B. mit dem Befehl new Date(originalesDatum.getTime()) erreicht werden.

Hier ist ein Beispiel, um das Datum in 1 Woche in der Zukunft zu berechnen:

```Javascript
let originalesDatum = new Date();
let zukunftsDatum = new Date(originalesDatum.getTime());
zukunftsDatum.setDate(zukunftsDatum.getDate() + 7);
console.log(zukunftsDatum);
```

Dies würde das Datum in 7 Tagen ab dem jetzigen Datum ausgeben. Man kann auch negative Werte verwenden, um ein Datum in der Vergangenheit zu berechnen, z.B. setDate(zukunftsDatum.getDate() - 7). Für eine tiefergehende Erklärung zu den verschiedenen Methoden und Funktionen der Date-Klasse empfehle ich einen Blick in die offizielle Javascript-Dokumentation.

## Tiefergehender Einblick

Wenn es darum geht, Datumsangaben in der Zukunft oder Vergangenheit zu berechnen, gibt es in Javascript auch Bibliotheken wie moment.js, die komplexere Funktionen und Optionen bieten. Diese können hilfreich sein, wenn es darum geht, verschiedene Zeitzonen oder Datum- und Zeitformate zu berücksichtigen. Es gibt auch Möglichkeiten, die Berechnung von Datumsangaben mit anderen Klassen oder Funktionen wie der Math-Klasse oder der setInterval() Funktion zu kombinieren, um automatisierte Aufgaben durchzuführen oder Animationen zu erstellen. Beim Experimentieren und Kombinieren dieser verschiedenen Methoden sind der Kreativität und den Anwendungsmöglichkeiten keine Grenzen gesetzt.

## Siehe auch

- Offizielle Javascript Date-Dokumentation: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Math-Klasse: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math
- setInterval() Funktion: https://developer.mozilla.org/de/docs/Web/API/WindowOrWorkerGlobalScope/setInterval