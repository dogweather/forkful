---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Javascript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann nützlich sein, um bestimmte Aufgaben in einer Anwendung zu automatisieren, wie z.B. das Anzeigen von zukünftigen Terminen oder das Berechnen von Fristen. Es kann auch hilfreich sein, um Zeitbereiche zu bestimmen, wie z.B. eine Woche oder ein Monat in der Zukunft.

## So geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, muss zunächst das aktuelle Datum und die gewünschte Anzahl an Tagen hinzugefügt oder subtrahiert werden. Hier ist ein einfaches Beispiel:

```Javascript
// Aktuelles Datum
let heute = new Date();

// Berechnung von Datum in 7 Tagen in der Zukunft
let zukunft = new Date(heute.getFullYear(), heute.getMonth(), heute.getDate() + 7);

console.log(zukunft); // Ausgabe: Datum in 7 Tagen in der Zukunft
```

Zuerst wird das aktuelle Datum mit der JavaScript `Date()` Funktion abgerufen. Dann wird mithilfe der `getFullYear()`, `getMonth()` und `getDate()` Methoden das Datum in eine variable gespeichert. Schließlich wird mit `+ 7` die gewünschte Anzahl an Tagen hinzugefügt und das Ergebnis wird in einer neuen `Date` Variable gespeichert.

Für die Berechnung von Daten in der Vergangenheit kann einfach `- 7` verwendet werden, um 7 Tage abzuziehen. Hier ist ein Beispiel:

```Javascript
// Aktuelles Datum
let heute = new Date();

// Berechnung von Datum vor 7 Tagen
let vergangenheit = new Date(heute.getFullYear(), heute.getMonth(), heute.getDate() - 7);

console.log(vergangenheit); // Ausgabe: Datum vor 7 Tagen
```

Es ist auch möglich, andere Zeiteinheiten wie Monate oder Jahre zu berechnen, indem man die entsprechenden Werte für `getMonth()` und `getFullYear()` anpasst.

## Tiefere Einblicke

Die JavaScript `Date` Funktion bietet viele Methoden und Eigenschaften, die hilfreich sind, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Zum Beispiel kann man die verschiedenen `get` und `set`-Methoden verwenden, um einzelne Teile eines Datums wie Tag, Monat, Jahr oder sogar die Zeit zu modifizieren. Auch das Verwenden von `toLocaleDateString()` kann helfen, um ein Datum in verschiedenen Formaten auszugeben.

Es ist wichtig zu beachten, dass JavaScript die Zeitzone des verwendeten Geräts verwendet. Wenn das korrekte Datum und die korrekte Zeitzone wichtig sind, ist es besser, auf externe Bibliotheken oder APIs zurückzugreifen.

## Siehe auch

- [MDN Web Docs - Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Hackernoon - Calculate Future Dates in JavaScript](https://hackernoon.com/calculate-future-dates-in-javascript-634eeeb23b21)