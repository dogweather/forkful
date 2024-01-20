---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få det aktuella datumet innebär att man tar reda på vilket datum det är just nu. Programmerare gör detta för att logga händelser, schemalägga uppgifter eller för att jämföra datum.

## Hur gör man:
Här är ett exempel på att få det aktuella datumet i Arduino:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
}

void loop () {
  DateTime now = rtc.now();
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
}
```
När du kör detta program, skriver det ut det nuvarande datumet i formatet ÅR/MÅNAD/DAG på serieporten.

## Fördjupning
Att få det aktuella datumet är en grundläggande funktion som har använts sedan början av mjukvaruutveckling. Arduino använder en RTC (Real Time Clock) modul för att hålla reda på tiden även när strömmen är avstängd.

Ett alternativ är att använda en tidsserver på internet, men detta kräver en internetanslutning, vilket kan vara opraktiskt för vissa Arduino-projekt.

De viktigaste sakerna att komma ihåg när du implementerar detta är att vara medveten om vilket tidformat du använder (12-timmar eller 24-timmar) och se till att RTC-modulen är korrekt inställd, eftersom den kan driva över tid.

## Se även
Här är några resurser som du kan kolla på för mer information:

- [Arduino's RTC Bibliotek](https://www.arduino.cc/reference/en/libraries/rtclib/)
- [RTC DS1307 chip information](https://datasheets.maximintegrated.com/en/ds/DS1307.pdf)
- [Arduino's inbyggda "Time" bibliotek](https://playground.arduino.cc/Code/time)