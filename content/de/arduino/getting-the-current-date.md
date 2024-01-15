---
title:                "Das aktuelle Datum erhalten"
html_title:           "Arduino: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum?

Die aktuelle Datumserfassung ist ein wesentliches Merkmal in vielen Anwendungen, insbesondere bei der Verwendung von Sensoren und Aktuatoren. Das Datum ermöglicht es uns, zeitliche Daten aufzuzeichnen und zu analysieren. Wenn Sie also in der Welt des Arduino-Programmierens unterwegs sind, ist es wichtig zu wissen, wie man das aktuelle Datum abruft.

## Wie geht das?

Um das aktuelle Datum abzurufen, müssen Sie zunächst die Bibliothek `RTClib` in Ihr Sketch einbinden. Diese Bibliothek ermöglicht es dem Arduino, mit einem Real-Time-Clock-Modul (RTC) zu kommunizieren, das das Datum und die Uhrzeit in Echtzeit aufbewahrt.

```Arduino
#include <Wire.h> // Wire-Bibliothek einbinden für I2C-Kommunikation
#include "RTClib.h" // RTClib-Bibliothek einbinden
```

Als nächstes müssen Sie ein RTC-Objekt initialisieren und eine Verbindung zu Ihrem RTC-Modul herstellen. Für dieses Beispiel verwenden wir das RTC-Modul DS3231.

```Arduino
RTC_DS3231 rtc; // Initialisiere das RTC-Objekt
```

Im `setup()` Teil Ihres Sketches müssen Sie die RTC-Verbindung herstellen, indem Sie `rtc.begin()` aufrufen. Wir können auch eine optionale Funktion `setDate()` verwenden, um das Datum manuell einzustellen, falls dies nicht bereits auf dem RTC-Modul eingestellt ist.

```Arduino
rtc.begin(); // RTC-Verbindung herstellen
// rtc.setDate(2021, 12, 31); - Manuelle Datumeinstellung (optional)
```

Um das aktuelle Datum abzurufen, können Sie die Funktion `rtc.now()` verwenden, die ein `DateTime`-Objekt zurückgibt. Dieses Objekt enthält Informationen über Datum und Uhrzeit, die mit verschiedenen Methoden abgerufen werden können.

```Arduino
DateTime now = rtc.now(); // Aktuelles Datum abrufen
int year = now.year(); // Jahr abrufen
int month = now.month(); // Monat abrufen
int day = now.day(); // Tag abrufen
```

Um das Datum im Seriellen Monitor anzuzeigen, können Sie die Funktion `Serial.print()` verwenden.

```Arduino
Serial.print("Aktuelles Datum: ");
Serial.println(now); // Datum im Format: YYYY-MM-DD
```

## Tiefer eintauchen

Das `DateTime`-Objekt hat noch mehr nützliche Methoden, die Sie beim Arbeiten mit dem Datum verwenden können. Zum Beispiel können Sie die Funktion `dayOfTheWeek()` verwenden, um den Wochentag abzurufen, oder `second()` für die Sekunden. Eine vollständige Liste aller verfügbaren Methoden finden Sie auf der offiziellen Dokumentationsseite der `RTClib`-Bibliothek.

## Siehe auch

- [Dokumentation der RTClib-Bibliothek](https://github.com/adafruit/RTClib/blob/master/README.md)
- [RTC DS3231 Modul kaufen](https://www.adafruit.com/product/255)
- [RTC DS3231 Modul Anschlussanleitung](https://learn.adafruit.com/ds3231-precision-rtc-breakout/circuitpython)