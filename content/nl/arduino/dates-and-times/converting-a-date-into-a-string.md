---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:32.117058-07:00
description: "Een datum naar een tekenreeks converteren betekent de weergave van een\
  \ datum veranderen van een formaat dat programmeren begrijpt, zoals dag-, maand-\
  \ en\u2026"
lastmod: '2024-03-13T22:44:51.083382-06:00'
model: gpt-4-0125-preview
summary: "Een datum naar een tekenreeks converteren betekent de weergave van een datum\
  \ veranderen van een formaat dat programmeren begrijpt, zoals dag-, maand- en\u2026"
title: Een datum converteren naar een string
weight: 28
---

## Wat & Waarom?

Een datum naar een tekenreeks converteren betekent de weergave van een datum veranderen van een formaat dat programmeren begrijpt, zoals dag-, maand- en jaar integers, naar platte tekst. We doen dit om datums weer te geven in een voor mensen leesbaar formaat of om voor te bereiden op opslag en later gebruik.

## Hoe:

Hier is een eenvoudige voorbeeld van het converteren van een datum naar een tekenreeks op Arduino:

```Arduino
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Kon RTC niet vinden");
    while (1);
  }
  
  DateTime now = rtc.now();
  char dateString[11]; // Genoeg ruimte voor "DD/MM/JJJJ"

  sprintf(dateString, "%02d/%02d/%04d", now.day(), now.month(), now.year());
  Serial.println(dateString);
}

void loop() {
  // Niet nodig om de conversie te herhalen.
}
```

Voorbeelduitvoer:

```
23/03/2023
```

## Diepere Duik

Historisch gezien is tijdrepresentatie een complex aspect van programmeren geweest vanwege verschillende formaten en tijdzones. Arduino's tijdgerelateerde functies nemen de complexiteit op zich, waardoor we ons kunnen concentreren op het begrijpen van de tijdgegevens.

Hoewel we de `RTClib` bibliotheek gebruikt hebben, bieden alternatieven zoals de `TimeLib.h` vergelijkbare functionaliteit. Het kiezen van een bibliotheek hangt af van voorkeur en specifieke functies, zoals ingebouwde tijdzoneafhandeling.

De sleutelfunctie `sprintf` die hier gebruikt wordt, formatteert de gegevens in een tekenreeks. Deze is gebaseerd op de standaard C bibliotheekfunctie, die robuust is maar geheugenintensief kan zijn voor complex gebruik. Een lichter, meer basisalternatief zou `snprintf` zijn, dat ervoor zorgt dat je de grootte van je buffer niet overschrijdt en veiliger is tegen bufferoverlopen.

## Zie Ook

- Arduino's Time bibliotheek: http://playground.arduino.cc/Code/Time 
- DateFormat: https://www.arduino.cc/reference/en/libraries/date-format/
- RTClib Documentatie: https://github.com/adafruit/RTClib
