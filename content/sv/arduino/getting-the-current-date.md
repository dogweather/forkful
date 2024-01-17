---
title:                "Att få nuvarande datum"
html_title:           "Arduino: Att få nuvarande datum"
simple_title:         "Att få nuvarande datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hämta den nuvarande datumen är en viktig del av många Arduino-program. Genom att få tillgång till det aktuella datumet kan vi göra ett brett utbud av uppgifter, från enkla tidshantering till mer avancerade schemaläggning av uppgifter.

## Hur man gör:

Här är ett exempel på kod som visar hur man kan få det nuvarande datumet med Arduino:

```Arduino
#include <RTClib.h>
#include <Wire.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();
  if (! rtc.isrunning()) {
     Serial.println("RTC är inte initierat!");
     rtc.adjust(DateTime(F(__DATE__), F(__TIME__))); //Denna kod ställer in RTC:t med kompileringstiden 
  }
}

void loop() {
  DateTime now = rtc.now(); 
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(' ');
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
 
  delay(3000);
}
```

Konsolen kommer att visa den nuvarande datum- och tidsinformationen som den erhålls från RTC-modulen.

## Djupdykning:

Det finns flera olika sätt att hämta datumet på med Arduino, inklusive att använda en RTC-modul som ovan, eller genom att ansluta till en ntp-server för att få det aktuella datumet från internet. Dessa alternativ är användbara för olika typer av projekt och beror på tillgängliga resurser och behov. Implementationen av koden kan också variera beroende på vilken RTC-modul som används.

## Se också:

- [Arduino referens för RTClib](https://www.arduino.cc/en/Reference/RTC)
- [Uppdatering av böcker med RTC DS3231](http://www.rinkydinkelectronics.com/library.php?id=73)
- [Använda en ntp-server med Arduino](https://www.arduino.cc/en/Tutorial/LibNTPClient)