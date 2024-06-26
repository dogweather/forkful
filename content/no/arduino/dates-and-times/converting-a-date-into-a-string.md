---
date: 2024-01-20 17:35:49.791070-07:00
description: "Hvordan: \xC5 konvertere dato til en streng har v\xE6rt en del av programmering\
  \ s\xE5 lenge vi har hatt grensesnitt som er tilpasset mennesker. I eldre systemer\u2026"
lastmod: '2024-04-05T22:50:55.075328-06:00'
model: gpt-4-1106-preview
summary: "\xC5 konvertere dato til en streng har v\xE6rt en del av programmering s\xE5\
  \ lenge vi har hatt grensesnitt som er tilpasset mennesker."
title: Konvertere en dato til en streng
weight: 28
---

## Hvordan:
```Arduino
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Kunne ikke finne RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC har mistet strømmen, setter klokken!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  char buf1[] = "YYMMDDhhmmss";
  now.toString(buf1);
  
  Serial.print("Dato som streng: ");
  Serial.println(buf1); // Skriv ut dato som streng i formatet ÅÅMMDDttmmss

  delay(1000);
}
```
Sample output:
```
Dato som streng: 210309142355
```

## Dykk dypere:
Å konvertere dato til en streng har vært en del av programmering så lenge vi har hatt grensesnitt som er tilpasset mennesker. I eldre systemer ble tidsstempling ofte lagret som strenger, men moderne praksis foretrekker mer kompakte og standardiserte dataformater som UNIX-tid, som enkelt kan konverteres til lesbar tekst etter behov.

Det finnes alternative måter å konvertere dato til en streng, som C++-bibliotekets `strftime` funksjon. På Arduino bruker vi ofte `RTClib` fordi det er lett å bruke med RTC (Real Time Clock)-moduler.

Implementasjonsdetaljer kan variere avhengig av hvilken tidsmodul du bruker. For eksempel, `RTC_DS3231`-modulen er veldig nøyaktig og holder tiden selv når Arduino er av, noe som også gir oss mulighet til å lagre og hente tiden selv etter strømbrudd.

## Se også:
- Arduino's `TimeLib` bibliotek: https://www.arduino.cc/playground/Code/Time
- `strftime` C++ funksjon dokumentasjon: http://www.cplusplus.com/reference/ctime/strftime/
- Mer om `RTC_DS3231` og RTClib: https://github.com/adafruit/RTClib
