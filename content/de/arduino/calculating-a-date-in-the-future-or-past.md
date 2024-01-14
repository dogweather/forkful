---
title:                "Arduino: Futur oder Vergangenheitsdatum berechnen"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Die Berechnung eines Datums in der Zukunft oder Vergangenheit kann hilfreich sein, um Zeitintervalle zu bestimmen oder Ereignisse zu planen. Mit Arduino ist dies möglich, und in diesem Blog-Beitrag werden wir uns ansehen, wie man dies umsetzen kann.

## How To

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, kann man die `DateTime` Bibliothek in Arduino verwenden. Diese stellt verschiedene Funktionen zur Verfügung, um mit Datum und Uhrzeit zu arbeiten.

```Arduino
#include <DateTime.h>

void setup() {
  // Datum und Uhrzeit festlegen (Jahr, Monat, Tag, Stunde, Minute, Sekunde)
  DateTime jetzt(2020, 04, 20, 12, 00, 00);

  // Datum in der Zukunft berechnen (z.B. 30 Tage später)
  DateTime zukunft = jetzt + TimeSpan(30, 0, 0, 0);
  // Datum in der Vergangenheit berechnen (z.B. 2 Monate und 5 Tage vorher)
  DateTime vergangenheit = jetzt - TimeSpan(0, 2, 5, 0);

  // Ausgabe des berechneten Datums (Tag, Monat, Jahr)
  Serial.print(zukunft.day()).print(".");
  Serial.print(zukunft.month()).print(".");
  Serial.println(zukunft.year());
  Serial.print(vergangenheit.day()).print(".");
  Serial.print(vergangenheit.month()).print(".");
  Serial.println(vergangenheit.year());
}

void loop() {

}
```

Der obige Code demonstriert, wie man mit der `DateTime` Bibliothek ein Datum in der Zukunft und Vergangenheit berechnen kann. Dabei wird zuerst ein `DateTime`-Objekt für das aktuelle Datum und die Uhrzeit erstellt. Anschließend kann man mithilfe der `TimeSpan` Klasse ein Zeitintervall angeben, welches auf das aktuelle Datum addiert oder subtrahiert werden soll. Das Ergebnis wird dann in einem neuen `DateTime`-Objekt gespeichert und kann auf verschiedene Arten ausgegeben werden.

## Deep Dive

Die `DateTime` Bibliothek basiert auf dem Unix-Zeitformat, welches die Anzahl der Sekunden seit dem 1. Januar 1970 um 00:00 Uhr UTC angibt. Dadurch ist es möglich, Datumsberechnungen mit hoher Genauigkeit durchzuführen. Die Bibliothek stellt neben den oben genannten Funktionen auch weitere nützliche Methoden zur Verfügung, um beispielsweise auf Wochentage oder Zeitintervalle zu prüfen.

Man sollte jedoch beachten, dass Arduino keine Batterie oder Echtzeituhr besitzt, um die Zeit auch bei einem Neustart des Systems beizubehalten. Daher muss man das aktuelle Datum und die Uhrzeit bei jedem Start des Systems manuell setzen oder ein externes Modul verwenden.

## Siehe auch

- [Offizielle Dokumentation der `DateTime` Bibliothek](https://github.com/PaulStoffregen/DateTime)
- [Beitrag im Arduino Forum zu Datumsberechnungen](https://forum.arduino.cc/index.php?topic=43462.0)
- [Tutorial zur Verwendung von Arduino mit einer Echtzeituhr](https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit?view=all)