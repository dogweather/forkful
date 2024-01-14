---
title:                "Arduino: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann für verschiedene Anwendungen nützlich sein, wie z.B. für die Steuerung von Zeitabläufen oder das Erstellen von Alarmen. Mit der Arduino-Programmierung können Sie solche Berechnungen bequem durchführen.

## So geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen Sie zuerst das aktuelle Datum mit den Funktionen "day()", "month()" und "year()" erhalten und in Variablen speichern. Anschließend können Sie die gewünschte Anzahl von Tagen zu diesem Datum hinzufügen oder davon abziehen, um das zukünftige oder vergangene Datum zu berechnen.

Hier ist ein Beispiel für die Berechnung des Datums in 30 Tagen:

```Arduino
int currentDay = day(); //speichert den aktuellen Tag in einer Variable
int currentMonth = month(); //speichert den aktuellen Monat in einer Variable
int currentYear = year(); //speichert das aktuelle Jahr in einer Variable

int futureDay = currentDay + 30; //berechnet den zukünftigen Tag
int futureMonth = currentMonth; //der Monat bleibt gleich
int futureYear = currentYear; //das Jahr bleibt gleich

//falls der zukünftige Tag größer als die Anzahl der Tage im Monat ist, wird das Datum entsprechend angepasst
if (futureDay > 30) {
  futureDay -= 30; //subtrahiert die Anzahl der Tage im Monat
  futureMonth++; //erhöht den Monat um eins
}

//falls der zukünftige Monat größer als 12 ist, wird auch das Jahr entsprechend angepasst
if (futureMonth > 12) {
  futureMonth = 1; //setzt den Monat auf 1 (Januar)
  futureYear++; //erhöht das Jahr um eins
}

//gibt das zukünftige Datum im seriellen Monitor aus
Serial.print("Das Datum in 30 Tagen ist: ");
Serial.print(futureDay);
Serial.print(".");
Serial.print(futureMonth);
Serial.print(".");
Serial.println(futureYear);
```

Der oben genannte Code kann natürlich an Ihre spezifischen Anforderungen und Bedürfnisse angepasst werden, je nachdem, welche Berechnung Sie durchführen möchten.

## Tiefergehende Informationen

Es gibt auch andere Möglichkeiten, einen Zeitraum zu einem Datum hinzuzufügen oder abzuziehen, wie z.B. die Verwendung der "Time" Bibliothek oder der "millis()" Funktion. Ebenso können Sie verschiedene Bedingungen einbauen, um auch mit Schaltjahren umzugehen.

Es ist wichtig zu beachten, dass die Arduino-Uhrzeit in Millisekunden seit dem Hochfahren des Boards gezählt wird und nicht auf das tatsächliche Datum und die Uhrzeit basiert. Daher kann es zu Ungenauigkeiten kommen, wenn das Board längere Zeit eingeschaltet bleibt. In diesem Fall müssen Sie die Uhrzeit möglicherweise über das Internet oder eine externe RTC (Real-Time-Clock) aktualisieren.

## Siehe auch

- [Using Time and Date in Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [Millisecond Time Library for Arduino](https://playground.arduino.cc/Code/Millis/)
- [How to Add Time and Date to Your Arduino Projects](https://www.makerguides.com/arduino-time-date-tutorial/)