---
title:                "Sammenlikning av to datoer"
date:                  2024-01-20T17:32:15.278550-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenlikning av to datoer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Hva og Hvorfor?)
Å sammenligne to datoer innebærer å se på om de er like, hvilken som kommer før eller tiden mellom dem. Programmerere gjør dette for å håndtere frister, tidslinjer og for å synkronisere hendelser.

## How to: (Hvordan:)
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

  DateTime now = rtc.now();
  DateTime eventDate = DateTime(2023, 4, 15, 10, 30, 0);  // År, måned, dag, time, minutt, sekund

  if (now < eventDate) {
    Serial.println("The event is ahead.");
  } else if (now == eventDate) {
    Serial.println("The event is happening now!");
  } else {
    Serial.println("The event has passed.");
  }
}

void loop() {
  // Dette eksempelet krever ikke en repeterende kodeblok.
}
```

Sample output:
```
The event is ahead.
```

## Deep Dive (Dypdykk)
Historisk var tidshåndtering i datasystemer primitiv, ofte uten støtte for tidszoner eller skuddår. Moderne mikrokontrollere, som de som Arduino bruker, støtter ofte eksterne Real-Time Clock (RTC)-moduler som DS3231 for mer presis tidshåndtering. En alternativ metode uten RTC-modul ville være å bruke millis()-funksjonen, men dette kan være mindre nøyaktig over lengre perioder. Ved implementasjon bør man ta hensyn til tidssonejusteringer og skuddår for å sikre nøyaktig dato-sammenligning.

## See Also (Se også)
- [RTClib Library GitHub](https://github.com/adafruit/RTClib)
- [Arduino's millis() function](https://www.arduino.cc/reference/en/language/functions/time/millis/)