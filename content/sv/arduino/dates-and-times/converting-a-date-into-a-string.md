---
date: 2024-01-20 17:35:55.061520-07:00
description: "Hur g\xF6r man: Exempelutskrift: 2023-03-14 21:45:58."
lastmod: '2024-04-05T21:53:39.515532-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

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
