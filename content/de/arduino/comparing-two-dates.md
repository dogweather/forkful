---
title:                "Arduino: Vergleich von zwei Daten"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist eine nützliche Fähigkeit in der Programmierung, insbesondere für Anfänger im Arduino-Bereich. Dieser Prozess ermöglicht es, zu überprüfen, ob eine bestimmte Bedingung erfüllt ist oder nicht, und je nachdem, welche Datei neuer ist, bestimmte Aktionen auszuführen.

## Wie man es macht

Um zwei Daten zu vergleichen, verwenden wir den Vergleichsoperator "<", der angibt, dass die erste Datei kleiner ist als die zweite Datei. Hier ist ein Beispielcode:

```Arduino
// Einrichten der beiden Daten als Variablen
int date1 = 20210101;
int date2 = 20201231;

// Vergleichen der Daten
if (date1 < date2) {
  Serial.println("Date 1 ist vor Date 2.");
} else if (date1 == date2) {
  Serial.println("Die Daten sind gleich.");
} else {
  Serial.println("Date 2 ist vor Date 1.");
}
```

Ausgabe: Date 1 ist vor Date 2.

In diesem Beispiel haben wir die Variablen "date1" und "date2" erstellt und zwei unterschiedliche Datumsangaben zugewiesen. Durch die Verwendung von "if-else" Anweisungen können wir überprüfen, welche Bedingung zutrifft und entsprechend eine Ausgabe erzeugen.

## Tiefer in die Materie eindringen

Es gibt verschiedene Möglichkeiten, wie man zwei Daten vergleichen kann, je nach Typ der Daten und der gewünschten Art des Vergleichs. In der obigen Beispielverwendung haben wir den Vergleichsoperator "<" verwendet, aber es gibt auch andere wie "<=", ">", ">=", "==" und "!=". Es ist wichtig zu beachten, dass der Vergleich von Datumsangaben in Form von ganzen Zahlen erfolgt, die in einem bestimmten Format notiert werden müssen, um Vergleiche möglich zu machen.

Eine weitere Sache, die man bei der Verwendung von Vergleichsoperatoren beachten sollte, ist, dass sie in Verbindung mit "if" und "else" verwendet werden können, um Bedingungen zu überprüfen, bevor bestimmte Aktionen ausgeführt werden. Dies kann nützlich sein, um Entscheidungen in einem Programmablauf zu treffen.

## Siehe auch

Weitere Informationen zum Vergleichen von Daten in Arduino finden Sie unter den folgenden Links:

- [Arduino Vergleichsoperatoren](https://www.arduino.cc/reference/de/language/structure/control-structure/if/)
- [Comparing Data Types in Arduino](https://resources.arduino.cc/en/arduino-compare-data-types)
- [Date and Time Functions in Arduino](https://www.arduino.cc/reference/en/libraries/time/)