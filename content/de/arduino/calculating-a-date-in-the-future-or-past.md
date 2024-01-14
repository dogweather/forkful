---
title:    "Arduino: Die Berechnung eines Datums in der Zukunft oder Vergangenheit."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Das Berechnen von zukünftigen oder vergangenen Daten kann hilfreich sein, um Termine oder Ereignisse zu planen oder zu überwachen. Mit Arduino können wir dies auf einfache Weise programmieren und unser Projekt flexibler gestalten.

## Wie geht das
Das Berechnen von zukünftigen oder vergangenen Daten in Arduino ist leicht zu programmieren und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel, wie wir das aktuelle Datum um eine Woche erhöhen können:

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;

// Initialisiere den Real-Time Clock
void setup() {
  Serial.begin(9600);

  // Starte den RTC und überprüfe die Verbindung
  if (!rtc.begin()) {
    Serial.println("RTC konnte nicht gefunden werden!");
    while (1);
  }

  // Setze das aktuelle Datum auf den 29. Juli 2021
  DateTime now (2021, 7, 29, 12, 0, 0);
  // Berechne das Datum um eine Woche erhöht
  DateTime future = now + TimeSpan(7,0,0,0);

  // Gib das zukünftige Datum aus
  Serial.print("Das zukünftige Datum ist: ");
  Serial.println(future.timestamp());
}

void loop() {
  // Hier können weitere Aktionen ausgeführt werden
}
```

Die Ausgabe der Seriellen Monitor zeigt uns nun das zukünftige Datum in Form eines Unix-Timestamps an. Natürlich können wir auch andere Berechnungen durchführen, wie das Finden des Datums nach einer bestimmten Anzahl von Tagen oder das Zurückgehen in der Zeit.

## Tiefer Einblick
Um das aktuelle Datum in einem festgelegten Format auszugeben, können wir die `now.year()`, `now.month()`, `now.day()` Funktionen verwenden. Diese geben uns die aktuellen Werte von Jahr, Monat und Tag zurück. Wir können dann diese Werte in einem String zusammenfügen und ausgeben.

Um das Datum zurückzugehen, müssen wir die berechnete Zeitspanne als negativen Wert übergeben. Zum Beispiel: `DateTime past = now + TimeSpan(-10,0,0,0);`

Es ist auch möglich, die Zeit zu berücksichtigen und so ein genaues Datum und Zeit anzugeben, z.B. `DateTime now (2021, 7, 29, 12, 30, 0);`

## Siehe auch
- [Offizielle Dokumentation zu Zeit- und Datumfunktionen in Arduino](https://www.arduino.cc/reference/en/libraries/rtclib/)
- [Tutorial zum Berechnen von zukünftigen oder vergangenen Daten in Arduino](https://maker.pro/arduino/projects/how-to-calculate-date-in-arduino)