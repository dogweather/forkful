---
date: 2024-01-20 17:32:16.945534-07:00
description: Wie geht das? Sample Output.
lastmod: '2024-04-05T21:53:56.039826-06:00'
model: gpt-4-1106-preview
summary: ''
title: Vergleich von zwei Daten
weight: 27
---

## Wie geht das?
```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(23, 59, 30, 12, 31, 2020); // Jahr 2020, 31. Dezember, 23:59:30

  tmElements_t dt1, dt2;
  breakTime(now(), dt1);
  dt2 = dt1;
  dt2.Year = 2021; // Setze es auf 1 Jahr später
  
  if (makeTime(dt1) < makeTime(dt2)) {
    Serial.println("Datum 1 ist früher als Datum 2.");
  } else {
    Serial.println("Datum 2 ist früher als Datum 1 oder die Daten sind gleich.");
  }
}

void loop() {
  // Hier könnten zeitbasierte Operationen stehen
}
```
Sample Output:
```
Datum 1 ist früher als Datum 2.
```

## Tiefgang
Der Vergleich von zwei Daten auf Arduino basiert nicht auf eingebauten Funktionen der Programmiersprache, sondern auf der `TimeLib.h`, einer Bibliothek, die komplexe Zeitberechnungen ermöglicht. Früher waren solche Operationen auf Mikrocontrollern schwer umzusetzen, aber mit modernen Bibliotheken wie der TimeLib lassen sich Zeit- und Datumsvergleiche relativ einfach durchführen. Alternativen wie `RTClib` existieren auch, falls spezielle Features oder Unterstützung für bestimmte Hardware nötig sind. Wichtig ist, dass die Bibliotheken das Unix Zeitstempelformat zur Berechnung nutzen: Sekunden seit dem 1. Januar 1970 (sogenannte POSIX-Zeit). Das macht die Vergleiche zuverlässig und unabhängig vom Format des angegebenen Datums.

## Siehe auch
- TimeLib Library: https://www.pjrc.com/teensy/td_libs_Time.html
- Arduino Time Library (TimeLib.h) Dokumentation: https://github.com/PaulStoffregen/Time
- RTClib (Echtzeituhr-Bibliothek): https://github.com/adafruit/RTClib
