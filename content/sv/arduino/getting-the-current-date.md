---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:13:07.303019-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta aktuellt datum innebär att du får reda på vilken dag det är just nu. Programmerare gör detta för att logga händelser, hantera tidbaserade funktioner eller bara visa datumet för användaren.

## Så här gör du:
Med Arduino och en RTC (real-time clock) modul som DS3231 kan du enkelt få aktuellt datum. Här kommer ett enkelt kodexempel:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Kunde inte hitta RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC förlorade ström, sätt aktuell tid!");
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
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
Exempel på utdata: `2023/3/17`

## Djupdykning:
En RTC-modul, som DS3231, använder ett litiumbatteri för att behålla tiden även när strömmen till huvudenheten (din Arduino) är avstängd. Liknande lösningar finns (som DS1307), men DS3231 är föredragen för sin noggrannhet och temperaturkompensation.

RTC-modulen kommunicerar med Arduino via I2C-protokollet, en tvåtråds seriell kommunikationsbuss som använder SDA (data linje) och SCL (klock linje). Implementering kan få ytterligare komplexitet om du behöver skapa tidbaserade händelseutlösare eller hantera tidszoner och skottsekunder.

## Se även:
- Arduino's officiella RTClib bibliotek: https://github.com/adafruit/RTClib
- DS3231 datablad för att förstå dess fullständiga kapacitet: https://datasheets.maximintegrated.com/en/ds/DS3231.pdf
- Wire biblioteket referens för I2C kommunikation: https://www.arduino.cc/en/Reference/Wire