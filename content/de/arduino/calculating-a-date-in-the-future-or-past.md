---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Arduino: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Berechnung eines zukünftigen oder vergangenen Datums ist ein Prozess, der eine spezifische Zeitspanne zu einem gegebenen Datum hinzufügt oder subtrahiert. Programmierer tun dies oft, um Aufgaben zu planen oder Zeitintervalle zu überwachen.

## Wie geht's:

Betrachten wir als erstes ein einfaches Beispiel, um eine Stunde zu der aktuellen Zeit hinzuzufügen.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(14, 00, 0, 1, 1, 2020); // Setzt die Zeit auf 14:00 Uhr, 1. Januar 2020
}

void loop() {
  time_t t = now();
  t += 3600; // Fügt eine Stunde hinzu
  
  Serial.print(hour(t));
  Serial.print(":");
  Serial.print(minute(t));
  Serial.print(":");
  Serial.println(second(t));
  
  delay(1000);
}
```

Die Ausgabe wäre:

```Arduino
15:0:0
```

## Vertiefung:

Historisch gesehen war die Berechnung von Datumsangaben eine schmerzhafte Angelegenheit, da mit vielen Variablen wie Schaltjahren, Zeitumstellungen usw. gearbeitet werden musste. Heute machen Bibliotheken wie TimeLib den Prozess viel einfacher.

Alternativ könnte man die RTC (Real Time Clock) Hardware mit integrierter Datums- und Zeitfunktion verwenden. Diese Methode benötigt jedoch zusätzliche Hardware.

Die Berechnung eines zukünftigen oder vergangenen Datums in Arduino beinhaltet die Umwandlung der Datums- und Zeitwerte in Sekunden seit der Unix-Ära (1. Januar 1970) und Hinzufügen oder Subtrahieren des gewünschten Intervalls.

## Siehe auch:

- [Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [DateTime Library](https://github.com/adafruit/Adafruit_RTClib)
- [NTP Client Library](https://github.com/arduino-libraries/NTPClient)