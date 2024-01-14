---
title:    "Javascript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Manchmal müssen wir in der Programmierung ein Datum in der Zukunft oder Vergangenheit berechnen. Das kann nützlich sein, um beispielsweise Termine oder Fristen zu planen.

## Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Javascript-Datei `Date` verwenden. Wir können zum Beispiel mit dem aktuellen Datum als Startpunkt arbeiten und dann bestimmte Tage, Wochen oder Monate hinzufügen oder subtrahieren. Hier ist ein Beispiel:

```Javascript
// Aktuelles Datum
let heute = new Date();

// Berechnen eines Datums in der Zukunft
let zukunft = new Date();
zukunft.setDate(heute.getDate() + 30); // Fügt 30 Tage hinzu
console.log(zukunft); // Output: 30 Tage in der Zukunft

// Berechnen eines Datums in der Vergangenheit
let vergangenes = new Date();
vergangenes.setMonth(heute.getMonth() - 6); // Subtrahiert 6 Monate
console.log(vergangenes); // Output: 6 Monate in der Vergangenheit
```

In diesem Beispiel haben wir `setDate` und `setMonth` verwendet, um das Datum zu berechnen. Es gibt jedoch auch andere Methoden, die wir verwenden können, je nachdem, welche Einheit wir hinzufügen oder subtrahieren möchten (z.B. `setFullYear`, `setHours`, etc.).

## Tiefer eintauchen

Es gibt auch komplexere Anwendungen für die Berechnung von zukünftigen oder vergangenen Daten. Zum Beispiel könnten wir ein Skript schreiben, das basierend auf Benutzereingaben ein Datum in der Zukunft oder Vergangenheit berechnet. Wir könnten auch Funktionen schreiben, die verschiedene Zeitzonen berücksichtigen oder eine bestimmte Anzahl von Arbeitstagen berechnen, anstatt einfach nur ein Datum zu addieren oder zu subtrahieren.

Es gibt also viele Möglichkeiten, wie wir die Berechnung von zukünftigen oder vergangenen Daten in Javascript nutzen können.

## Siehe auch

- [MDN Web Docs: Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [StackOverflow: How to calculate date difference in JavaScript](https://stackoverflow.com/questions/3224834/how-to-calculate-date-difference-in-javascript)