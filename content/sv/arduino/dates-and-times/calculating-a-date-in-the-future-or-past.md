---
title:                "Beräkna ett datum i framtiden eller förflutet"
date:                  2024-01-20T17:28:40.163650-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutet"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett framtida eller förflutet datum innebär att man räknar ut ett datum utifrån ett givet startdatum och en tidsspann. Programmerare gör detta för att hantera tidsbaserade händelser, som utgångsdatum eller påminnelser.

## Hur man gör:
Med Arduino kan du inte direkt hantera datum utan hjälp från externa bibliotek. RTC (real-time clock) moduler och bibliotek som `RTClib` är användbara. Här är ett exempel med en RTC-modul:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void adjustDate(DateTime& current, int daysToAdd) {
  current = current + TimeSpan(daysToAdd);
}

void loop() {
  DateTime now = rtc.now();
  
  // Adjusting date by 7 days
  adjustDate(now, 7);
  
  // Print new date
  Serial.print("New date: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(10000); //Adjusting only every 10 seconds for this example
}
```

## Djupdykning:
Förr hanterades datum och tid ofta av operativsystemet eller inbyggda funktioner i programmeringsspråken. Men inom inbyggda system, som Arduino, krävs ytterligare komponenter eftersom basenheten inte håller koll på tid. RTC-moduler som DS3231 använder batterier för att behålla tiden även när strömmen är av. Alternativ som `TimeLib.h` finns, men de kan behöva manuell synkronisering. Vid implementation får man väga precision mot resurser – RTC-moduler är noggranna men tar fysiskt utrymme och energi.

## Se även:
- [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
- [RTClib GitHub Repository](https://github.com/adafruit/RTClib)
- [DS3231 RTC Module Datasheet](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf)
