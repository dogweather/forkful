---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:34:26.490172-07:00
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av en dato fra en streng innebærer å konvertere tekst til et datoformat programmet kan forstå og bruke. Vi gjør det for å enkelt håndtere datofunksjoner som lagring, sammenligning og beregninger.

## Hvordan:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Kunne ikke finne RTC");
    while (1);
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
**Eksempelutskrift:**
```
2023/03/01
2023/03/02
2023/03/03
```

## Dypdykk
Historisk har datoparsing vært utfordrende på grunn av ulike formater og tidssoner. Alternativer til RTC-biblioteket inkluderer Time.h eller innebygde funksjoner som `strptime()`. Viktige implementeringsdetaljer innebærer å ta hensyn til skuddår og lokal tidssonejustering.

## Se Også
- Arduino Time Library: http://playground.arduino.cc/Code/Time
- RTClib GitHub Repository: https://github.com/adafruit/RTClib
- Arduino støttedokumentasjon for `strptime()`: https://www.arduino.cc/reference/en/
