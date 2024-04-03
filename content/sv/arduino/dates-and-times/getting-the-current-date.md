---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:49.696908-07:00
description: "Hur man g\xF6r: Arduino i sig har ingen inbyggd metod f\xF6r att direkt\
  \ h\xE4mta det aktuella datumet, eftersom den saknar en realtidsklocka (RTC). Detta\
  \ kan dock\u2026"
lastmod: '2024-03-13T22:44:38.177737-06:00'
model: gpt-4-0125-preview
summary: "Arduino i sig har ingen inbyggd metod f\xF6r att direkt h\xE4mta det aktuella\
  \ datumet, eftersom den saknar en realtidsklocka (RTC)."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur man gör:
Arduino i sig har ingen inbyggd metod för att direkt hämta det aktuella datumet, eftersom den saknar en realtidsklocka (RTC). Detta kan dock åstadkommas genom att använda externa RTC-moduler som DS3231 och bibliotek såsom `RTClib`, utvecklat av Adafruit, vilket gör det enkelt att koppla upp sig mot dessa moduler.

Först, se till att `RTClib`-biblioteket är installerat i din Arduino IDE. Koppla sedan din RTC-modul till din Arduino enligt dess dokumentation.

Här är ett enkelt exempel för att komma igång:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Kunde inte hitta RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC förlorade ström, låt oss ställa in tiden!");
    // När tiden behöver ställas in på en ny enhet eller efter en strömförlust, kan du göra det här.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Aktuellt datum: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Fördröjning med 3 sekunder för att minska seriens spam
}
```

Exempel på utdata (förutsatt att din RTC har ställts in tidigare):

```
Aktuellt datum: 2023/4/15
```

Denna kod initierar RTC-modulen och hämtar sedan och skriver ut det aktuella datumet till seriemotorn var 3:e sekund. Kom ihåg, linjen `rtc.adjust(...)` kan avkommenteras och modifieras för att initialt eller efter ett strömbortfall ställa in RTC:ns datum och tid.
