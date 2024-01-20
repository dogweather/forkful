---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:13:13.077196-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente gjeldende dato betyr å få tak i dagens kalenderdato. Programmerere gjør dette for å merke hendelser, styre tidssensitive funksjoner, eller bare for å vise dato for brukere.

## Hvordan:
For å hente nåværende dato i Arduino, trenger du et RTC (Real Time Clock) modul. Her er et eksempel med en DS3231 RTC:

```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  
  delay(1000);
}
```

Eksempelutdata:
```
2023/3/15
```

## Dypdykk:
Å holde styr på tid og dato har vært essensielt siden de første datamaskinene. RTC-moduler som DS3231 beholder tid selv uten mikrokontrollerens hjelp, takket være et lite batteri. Alternativer inkluderer nettverksbaserte tidsinnhentinger via NTP (Network Time Protocol), men RTC-moduler gir deg offline funksjonalitet. Implementeringen av datofunksjonene i Arduino krever at du holder biblioteker oppdatert og forstår hvordan tiden vedlikeholdes i din spesifikke RTC-modul.

## Se Også:

- [RTClib Library GitHub](https://github.com/adafruit/RTClib)