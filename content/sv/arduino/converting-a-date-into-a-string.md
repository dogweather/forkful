---
title:                "Omvandla ett datum till en sträng"
date:                  2024-01-20T17:35:55.061520-07:00
model:                 gpt-4-1106-preview
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng innebär att omvandla tidsdata till läsbart textformat. Programmerare gör detta för att visa datum på skärmar eller för att logga händelser i system.

## Hur gör man:
```Arduino
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
    Serial.println("RTC lost power, setting the time!");
    // När tiden är satt en gång behövs inte dessa rader om klockan får ström
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  char dateStr[20];
  sprintf(dateStr, "%d-%02d-%02d %02d:%02d:%02d", now.year(), now.month(), now.day(), now.hour(), now.minute(), now.second());
  Serial.println(dateStr);
  delay(1000);
}
```
Exempelutskrift: 2023-03-14 21:45:58

## Djupdykning:
Att omvandla datum till strängar har blivit standard för att hantera datum i programmering sedan tidiga datorer. Andra metoder, som tidsstämplar och inbyggda datumfunktioner, finns också. Arduino använder `sprintf` för att formattera datum som strängar. Det här är kraftfullt eftersom du kan bestämma precis hur datumet ska visas, men det kräver också förståelse för `sprintf`-syntax.

## Se även:
- Arduino's Time Library: https://www.arduino.cc/en/Reference/Time
- RTClib, en populär bibliotek för tidskretsar: https://github.com/adafruit/RTClib
- strftime-funktion för formatering av datum och tid: http://www.cplusplus.com/reference/ctime/strftime/