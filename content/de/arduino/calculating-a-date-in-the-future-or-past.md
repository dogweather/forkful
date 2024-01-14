---
title:    "Arduino: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum
Du fragst dich vielleicht, warum es wichtig ist, ein Datum in der Zukunft oder Vergangenheit zu berechnen. Nun, es gibt viele mögliche Gründe, warum man das tun möchte. Zum Beispiel könnte es sein, dass du ein Projekt mit einer Aufgabenplanung hast und wissen möchtest, an welchem Tag eine bestimmte Aufgabe erledigt werden muss. Oder vielleicht möchtest du ein Countdown-Timer für ein bevorstehendes Ereignis programmieren. Wie auch immer der Grund sein mag, die Fähigkeit, ein Datum in der Zukunft oder Vergangenheit berechnen zu können, ist eine nützliche Fähigkeit für jeden Arduino-Programmierer.

## Wie man es macht
Das Berechnen eines Datums in der Zukunft oder Vergangenheit erfordert etwas Programmierung, aber mit Hilfe von Arduino ist es gar nicht so schwer. Im Folgenden werde ich dir zeigen, wie du ein Datum in der Zukunft oder Vergangenheit berechnen und ausgeben kannst.

### Schritt 1: Initialisierung der Variablen
Definiere zunächst die Variablen, die du für die Berechnung des Datums benötigst. Dazu gehören das aktuelle Datum, die Anzahl der Tage in der Zukunft oder Vergangenheit, die du berechnen möchtest, und das berechnete Datum.

```Arduino
int currentDay, currentMonth, currentYear; //Aktuelles Datum
int numOfDays; //Anzahl der Tage in der Zukunft oder Vergangenheit
int calculatedDay, calculatedMonth, calculatedYear; //Berechnetes Datum
```

### Schritt 2: Lesen des aktuellen Datums
Verwende die `day()`, `month()` und `year()` Funktionen, um das aktuelle Datum auszulesen und speichere die Werte in den entsprechenden Variablen.

```Arduino
currentDay = day();
currentMonth = month();
currentYear = year();
```

### Schritt 3: Berechnung des Datums
Um das Datum in der Zukunft oder Vergangenheit zu berechnen, addiere oder subtrahiere einfach die Anzahl der Tage von dem aktuellen Datum. Beachte dabei, dass sich die Anzahl der Tage auf das Datum bezieht und nicht auf den Monat.

Berechnung in der Zukunft:
```Arduino
calculatedDay = currentDay + numOfDays;
calculatedMonth = currentMonth;
calculatedYear = currentYear;
```

Berechnung in der Vergangenheit:
```Arduino
calculatedDay = currentDay - numOfDays;
calculatedMonth = currentMonth;
calculatedYear = currentYear;
```

### Schritt 4: Überprüfung und Anpassung des berechneten Datums
Sollte das berechnete Datum außerhalb der Grenzen eines Monats oder Jahres liegen (z.B. 31. Februar oder 13. Monat), musst du dies berücksichtigen und das Datum entsprechend anpassen. Hier ist ein Beispiel für die Anpassung des Datums in Overshoot-Situationen:

```Arduino
if (calculatedDay > 31) {
  calculatedDay = calculatedDay - 31;
  calculatedMonth++;
}
if (calculatedMonth > 12) {
  calculatedMonth = calculatedMonth - 12;
  calculatedYear++;
}
```

### Schritt 5: Ausgabe des berechneten Datums
Jetzt kannst du das berechnete Datum auf dem seriellen Monitor ausgeben, um es zu überprüfen. Hier ist ein Beispiel dafür, wie du das Datum im Format TT.MM.JJJJ ausgeben kannst:

```Arduino
Serial.print(calculatedDay);
Serial.print(".");
Serial.print(calculatedMonth);
Serial.print(".");
Serial.println(calculatedYear);
```

## Tiefer Einblick
Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann kompliziert sein, wenn man es genauer betrachtet. Es gibt viele verschiedene Faktoren, die berücksichtigt werden müssen, wie zum Beispiel Schaltjahre, Monatslängen und das Überschreiten von Monats- oder Jahresgrenzen. Ein tieferes Verständnis der Berechnung von Daten ist hilfreich, um mögliche Fehler zu vermeiden und genaue Ergebnisse zu erzielen.

## Siehe auch
- [Arduino Reference - day()](https://www.arduino.cc/reference/en/language/variables/data-types/day/)
- [Arduino Reference - month()](https://www.arduino.cc/reference/en/language/variables/data-types/month/)
- [Arduino Reference - year()](https://www.arduino.cc/reference/en