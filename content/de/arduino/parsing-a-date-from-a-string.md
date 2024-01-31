---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:34:14.848921-07:00
simple_title:         "Datum aus einem String parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum-String-Parsing bedeutet, einen Text mit einem Datum in eine Struktur umzuwandeln, die der Computer versteht. Programmierer machen das, um Daten zu verarbeiten, zu vergleichen und umzuwandeln.

## How to:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC nicht gefunden!");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC hat die Zeit verloren!");
    // RTC mit Datum & Uhrzeit der Kompilation setzen:
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime jetzt = rtc.now();

  Serial.print("Aktuelles Datum & Uhrzeit: ");
  Serial.print(jetzt.year(), DEC);
  Serial.print('/');
  Serial.print(jetzt.month(), DEC);
  Serial.print('/');
  Serial.print(jetzt.day(), DEC);
  Serial.print(" ");
  Serial.print(jetzt.hour(), DEC);
  Serial.print(':');
  Serial.print(jetzt.minute(), DEC);
  Serial.print(':');
  Serial.print(jetzt.second(), DEC);
  Serial.println();

  delay(1000);
}
```
Ausgabe:
```
Aktuelles Datum & Uhrzeit: 2023/3/15 12:45:30
```

## Deep Dive
Das Parsing von Datum-Strings ist nicht neu. Frühe Rechner benutzten ähnliche Methoden. Heute gibt es viele Bibliotheken (z.B. `RTClib` für Arduino), die das Handling vereinfachen. Implementierungsdetails hängen von der gegebenen Bibliothek ab. Alternativen zum manuellen Parsing sind fertige Time-Management-Bibliotheken, die diesen Prozess abstrahieren.

## See Also
- [Arduino Time Library](https://github.com/PaulStoffregen/Time)
- [RTClib (eine Echtzeituhr-Bibliothek für Arduino)](https://github.com/adafruit/RTClib)
