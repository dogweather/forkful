---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Arduino-Datumsabfrage: Der ultimative Leitfaden

## Was ist das und warum?

Die Abfrage des aktuellen Datums ermöglicht es Arduino-Programmierern, aktuelle Daten und Uhrzeit in ihren Projekten zu verwenden. Dies ist besonders nützlich für Protokollierungszwecke, Zeitmessungen und ereignisbasierte Funktionen.

## Wie macht man es:

Die RTC (Real Time Clock) Bibliothek für Arduino ermöglicht es uns, das aktuelle Datum und die Uhrzeit abzurufen.

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
  }
}

void loop () {
    DateTime now = rtc.now();
    
    Serial.print(now.year(), DEC);
    Serial.print('/');
    Serial.print(now.month(), DEC);
    Serial.print('/');
    Serial.print(now.day(), DEC);
    Serial.print(' ');
    delay(3000);
}
```

Dieser Sketch wird in Ihrer Seriellen Konsole ein Datum im Format JJJJ/MM/DD ausgeben.

## Tiefere Einblicke

Historisch gesehen konnten Arduino Boards ohne zusätzliche Hardware keine Echtzeitinformationen bereitstellen. Aus diesem Grund haben Entwickler Echtzeituhrmodule (RTC) entwickelt, die über die I2C-Schnittstelle mit dem Arduino Board verbunden werden können.

Es gibt Alternativen zur RTC-Bibliothek, wie z.B. die TimeLib.h. Diese Bibliothek ist jedoch komplexer und bietet Funktionen, die über die Bedürfnisse der meisten einfachen Projekte hinausgehen.

Die RTC-Bibliothek greift auf die systemeigene I2C-Schnittstelle des Arduino zu. Das Modul übermittelt die aktuelle Uhrzeit durch eine Kombination aus integriertem Oszillator und Batterie.

## Siehe auch:

- RTClib Bibliothek: https://github.com/adafruit/RTClib
- Einführung in das I2C-Protokoll: http://www.i2c-bus.org/
- Adafruit DS1307 Real Time Clock Assembled Breakout Board: https://www.adafruit.com/product/3296