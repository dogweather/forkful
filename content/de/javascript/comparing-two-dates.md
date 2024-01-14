---
title:                "Javascript: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleichen von zwei Daten in der Programmierung kann sehr nützlich sein, um zum Beispiel festzustellen, welches Datum später oder früher ist, oder um Verzögerungen zu berechnen.

## Wie geht es?

Um zwei Daten zu vergleichen, können wir die eingebaute `Date`-Klasse in Javascript verwenden. Diese Klasse ermöglicht es uns, Datumswerte zu erstellen, zu vergleichen und zu manipulieren.

Um zwei Daten zu vergleichen, müssen wir zunächst zwei `Date`-Objekte erstellen. Hier ist ein Beispiel, um den heutigen Tag als Datum zu erstellen:

```Javascript
const heute = new Date();
```

Um ein bestimmtes Datum zu erstellen, können wir die `Date`-Klasse auch mit einer Datumszeichenfolge als Argument verwenden. Zum Beispiel:

```Javascript
const geburtstag = new Date("1990-05-12");
```

Jetzt haben wir beide Datenobjekte erstellt, können wir sie miteinander vergleichen. Hier sind einige Beispiele, die verschiedene Vergleiche durchführen und das Ergebnis als Konsolenausgabe anzeigen:

```Javascript
// Vergleiche ob das Geburtsdatum vor heute liegt
console.log(geburtstag < heute); // output: true

// Vergleiche ob das Geburtsdatum nach dem heutigen Tag ist
console.log(geburtstag > heute); // output: false

// Vergleiche ob beide Daten gleich sind
console.log(geburtstag === heute); // output: false
```

Wie wir sehen können, kehrt der Vergleichsoperator `>` oder `<` `true` oder `false` zurück, abhängig davon, ob das erste Datum früher oder später als das zweite ist. Der `===`-Operator prüft, ob beide Daten genau gleich sind.

## Deep Dive

Um zwei Daten in Javascript zu vergleichen, berücksichtigt die `Date`-Klasse mehrere Faktoren, wie die Zeitzone und die Tageszeit. Dies kann manchmal zu unerwarteten Ergebnissen führen, insbesondere wenn man nicht genau versteht, wie die Vergleiche durchgeführt werden.

Beispielsweise kann ein Datum mit einer Zeitangabe `13:00` auf einem Computer in Deutschland möglicherweise früher liegen als ein Datum mit der gleichen Zeit auf einem Computer in den USA, aufgrund der Zeitverschiebung.

Um diese unerwarteten Ergebnisse zu vermeiden, können wir die Methoden `getDate()` und `getUTCDate()` verwenden, um nur das Datum ohne Berücksichtigung der Zeitzone zu vergleichen. Dies würde wie folgt aussehen:

```Javascript
// Vergleiche ob beide Daten gleich sind, ohne die Zeitzone zu berücksichtigen
console.log(geburtstag.getUTCDate() === heute.getUTCDate()); // output: true
```

## Siehe auch

- [MDN Web Docs - Comparison operators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comparison_Operators)
- [MDN Web Docs - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)