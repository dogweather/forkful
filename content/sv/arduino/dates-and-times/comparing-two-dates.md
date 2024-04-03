---
date: 2024-01-20 17:32:11.403089-07:00
description: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att bed\xF6ma deras ordning\
  \ i tiden och avg\xF6ra tidsintervaller. Programmerare g\xF6r detta f\xF6r att hantera\
  \ och utv\xE4rdera\u2026"
lastmod: '2024-03-13T22:44:38.179825-06:00'
model: gpt-4-1106-preview
summary: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att bed\xF6ma deras ordning i\
  \ tiden och avg\xF6ra tidsintervaller."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## Hur gör man:
```arduino
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  DateTime now = rtc.now();
  DateTime deadline(2023, 4, 30, 23, 59, 59);

  if (now < deadline) {
    Serial.println("Deadline not passed");
  } else if (now == deadline) {
    Serial.println("Deadline is right now");
  } else {
    Serial.println("Deadline has passed");
  }
}

void loop() {
  // Main loop does nothing in this example.
}
```
Exempelutmatning:
```
Deadline not passed
```

## Fördjupning
Att jämföra datum är ett ämne så gammalt som programmering själv. Innan bibliotek som RTClib fanns, var datumhantering komplex och felbenägen. Alternativ till RTClib inkluderar TimeLib och inbyggda funktioner i många databashanterare. Viktiga detaljer i implementationen inkluderar att hantera skottår, tidszoner och övergångar till/från sommartid.

## Se även
- RTClib dokumentation: https://github.com/adafruit/RTClib
- Arduino Time Library: https://www.arduino.cc/en/Reference/Time
- Artikel om tidshantering och problematik: https://www.arduino.cc/en/Tutorial/BuiltInExamples/DateTime
