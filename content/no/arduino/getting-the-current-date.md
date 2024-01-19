---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente den nåværende datoen er å få datoen på dette nøyaktige tidspunktet fra systemet. Programmerere gjør det for å merke data, loggføre hendelser, eller kanskje sette en tidsfrist.

## Hvordan:

Arduino har ingen innebygd funksjon for å hente nåværende dato, men vi kan bruke en RTC-modul (Real Time Clock). Her er et eksempel på hvordan du kan bruke DS3231 RTC-modulen med Arduino.

```Arduino
#include <DS3231.h>

DS3231  rtc(SDA, SCL);

void setup()
{
  rtc.begin();
}

void loop()
{
  Serial.print(rtc.getDateStr());
  delay(1000);
}
```
Så når du laster ned og kjører dette programmet, vil utskriften på serielporten se slik ut:

```Arduino
2023-05-04 
2023-05-04
...
```

## Deep Dive

Historisk sett, siden Arduinos ikke har en innebygd klokke, har de alltid vært avhengige av eksterne moduler for å få sanntidsdata. Det finnes flere alternative moduler, som DS1307, som også kan gi deg nåværende tid og dato. Disse modulene lagrer tidspunktet selv når Arduino er slått av og holdes nøyaktige ved hjelp av et lite klokkebatteri. 

Å hente den nåværende datoen med en RTC-modul er ganske rett fram, men krever riktig kobling og oppsett av modulen. For DS3231 er SDA og SCL (I2C) tilkoblet for kommunikasjon med Arduino. I eksemplet ovenfor er rtc.getDateStr() koden som henter og returnerer en streng med datoen.

## Se Også

For å lære mer om dette, kan du lese disse:
* [DS3231 RTC modul (Arduino Offisiell Side)](https://store.arduino.cc/usa/arduino-rtc-ds1307)
* [RTC moduler og Arduino (Artikkel)](http://www.hobbytronics.co.uk/arduino-tutorial9-real-time-clock)