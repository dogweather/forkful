---
title:                "Javascript: Vergleich von zwei Datumsangaben"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumswerten ist ein nützliches Werkzeug in der Javascript Programmierung, um zu überprüfen, welches Datum früher oder später liegt. Dies ist besonders hilfreich, wenn man komplexe Zeitberechnungen durchführen muss oder mit Ereignissen arbeitet, die zu bestimmten Datumswerten gehören.

## Wie man es macht

Um zwei Datumswerte in Javascript zu vergleichen, gibt es mehrere Möglichkeiten. Eine Möglichkeit ist die Verwendung der `Date()` Funktion, die ein Datum-Objekt erstellt. Im folgenden Beispiel werden zwei mögliche Eingaben als Argumente für die `Date()` Funktion verwendet. Diese werden dann verglichen und das Ergebnis wird in der Konsole ausgegeben.

```Javascript
var date1 = new Date("June 1, 2021");
var date2 = new Date("July 1, 2021");

if (date1 > date2) {
  console.log("Date1 ist später als Date2.");
} else if (date1 < date2) {
  console.log("Date1 ist früher als Date2.");
} else {
  console.log("Beide Daten sind gleich.");
}
```

Das obige Beispiel zeigt, wie man mit dem Vergleichsoperator `>` und `<` arbeiten kann, um zwei Datumswerte zu vergleichen. Die Ausgabe für dieses Beispiel wäre `Date1 ist früher als Date2.`

Man kann auch eine andere Methode namens `getTime()` verwenden, um das Datum in Millisekunden zu konvertieren und dann zu vergleichen. Hier ist ein Beispiel:

```Javascript
var date1 = new Date("2021-01-01");
var date2 = new Date("2021-01-02");

if (date1.getTime() > date2.getTime()) {
  console.log("Date1 ist später als Date2.");
} else if (date1.getTime() < date2.getTime()) {
  console.log("Date1 ist früher als Date2.");
} else {
  console.log("Beide Daten sind gleich.");
}
```

In diesem Beispiel wird das Datum in das ISO-Format (Jahr-Monat-Tag) umgewandelt und dann mit der `getTime()` Methode in Millisekunden konvertiert. Die Ausgabe für dieses Beispiel wäre `Date1 ist früher als Date2.`

## Tiefgehende Information

Wenn man zwei Datumswerte in Javascript vergleicht, werden sie als Objekte behandelt. Wenn die Vergleichsoperatoren verwendet werden, vergleicht Javascript die Zeitstempel der beiden Datumswerte. Dies kann zu ungenauen Ergebnissen führen, wenn man nicht auf die Zeitzone und die Sommer-/Winterzeit achten.

Um dies zu vermeiden, ist es ratsam, die Zeit in UTC (koordinierte Weltzeit) zu konvertieren. Dies kann durch die Verwendung von Methoden wie `getUTCFullYear()`, `getUTCMonth()`, `getUTCDate()` und so weiter erreicht werden. Diese Methoden geben die UTC-Zeit zurück, anstatt die lokale Zeit zu verwenden.

## Siehe auch

- [Javascript Date Referenz](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Javascript Vergleichsoperatoren](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Operators/Vergleichsoperatoren)