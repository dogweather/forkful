---
title:                "Eine Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Arduino: Eine Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Eine Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Was & Warum?

Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine wichtige Funktion in der Programmierung, um genaue Abläufe zu planen und Ereignisse vorherzusagen. Programmierer nutzen diese Funktion, um zukünftige Ereignisse zu planen oder vergangene Ereignisse zu analysieren und besser zu verstehen.

Wie geht's?

Hier sind Beispiele für die Berechnung eines Datums in der Zukunft und Vergangenheit mit dem aktuellen Arduino.

```Arduino
// Berechnung eines Datums in der Zukunft
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  
  // Aktuelles Datum und Zeit einstellen
  setTime(12, 30, 30, 23, 8, 2021);
  
  // Datumsberechnung
  int future_day = day() + 7; // 7 Tage in der Zukunft
  int future_month = 8; // September
  int future_year = year(); // aktuelles Jahr
  
  // Ausgabe
  Serial.print("Das Datum in 7 Tagen ist: ");
  Serial.print(future_month);
  Serial.print("/");
  Serial.print(future_day);
  Serial.print("/");
  Serial.println(future_year);
}

void loop() {}
```

```Arduino
// Berechnung eines Datums in der Vergangenheit
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  
  // Aktuelles Datum und Zeit einstellen
  setTime(12, 30, 30, 23, 8, 2021);
  
  // Datumsberechnung
  int past_day = day() - 10; // 10 Tage in der Vergangenheit
  int past_month = 7; // Juli
  int past_year = year(); // aktuelles Jahr
  
  // Ausgabe
  Serial.print("Das Datum vor 10 Tagen war: ");
  Serial.print(past_month);
  Serial.print("/");
  Serial.print(past_day);
  Serial.print("/");
  Serial.println(past_year);
}

void loop() {}
```

Vertiefung

Die Berechnung von Datumsangaben ist ein wichtiger Bestandteil der Programmierung und wird schon seit langem genutzt. Alternativ können auch Datum- und Zeitbibliotheken verwendet werden, welche jedoch möglicherweise spezielle Funktionen bieten und eine andere Syntax verwenden.

Weitere Informationen und Funktionen zur Arbeit mit Datum und Zeit finden Sie in der offiziellen TimeLib-Dokumentation. [Link zur Dokumentation](https://github.com/PaulStoffregen/Time)

Siehe auch

- [Vergleich von Datum- und Zeitbibliotheken](https://arduino-projekte.info/zeit-datum-arduino-einfach-verst%c3%a4ndlich/)
- [Berechnung von Zeitintervallen in der Programmierung](https://www.instructables.com/How-to-Calculate-Time-Intervals-in-Arduino/)
- [Praktische Anwendung von Datum und Zeit in der Realität](https://www.geekstips.com/arduino-date-time-timestamp-arhus-real-time-clock/)