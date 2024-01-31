---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:13:07.107280-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums auf einem Arduino zeigt dir das heutige Datum. Das ist nützlich für Zeitstempel, Logger, Uhrenanwendungen oder fürs Event-Management.

## How to:
Um das aktuelle Datum auf einem Arduino zu erhalten, verwenden wir ein RTC (Real Time Clock) Modul wie das DS3231. Hier ist ein einfaches Beispiel, das die Zeit ausliest und ausgibt.

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
    // Hier könntest du die Zeit mit rtc.adjust(...) neu setzen.
  }
}

void loop() {
  DateTime now = rtc.now();
  Serial.print(now.day());
  Serial.print('.');
  Serial.print(now.month());
  Serial.print('.');
  Serial.println(now.year());
  delay(1000); // Update jede Sekunde
}
```
Beispielausgabe:
```
17.3.2023
```

## Deep Dive
In den frühen Tagen von Mikrocontrollern war das Abrufen der aktuellen Zeit nicht direkt möglich. RTC-Module lösten dieses Problem. Alternativen zum DS3231 sind z.B. das DS1307 oder Internet-basierte Zeit-Services mittels eines ESP8266.

Der Schlüssel zu solch einem System ist die RTC-Bibliothek (`RTClib.h`), die verschiedene Funktionen zur Interaktion mit dem RTC-Modul bietet. Sie wandelt beispielsweise die Zeit in ein benutzerfreundliches Format um. Wichtig ist, bei Projekten, die auf die genaue Zeit angewiesen sind, die Batterie des RTC-Moduls im Auge zu behalten, da ein Stromverlust die Datum- und Zeitinformationen verliert.

## See Also
- RTClib Bibliothek: https://github.com/adafruit/RTClib
- Arduino Zeitbibliothek (`TimeLib.h`): https://www.pjrc.com/teensy/td_libs_Time.html
- NTP-Zeit synchronisieren mit ESP8266: https://randomnerdtutorials.com/esp8266-nodemcu-date-time-ntp-client-server-arduino/
