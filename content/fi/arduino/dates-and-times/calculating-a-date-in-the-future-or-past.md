---
date: 2024-01-20 17:31:03.329616-07:00
description: "How to: Arduino ei suoraan tue p\xE4iv\xE4m\xE4\xE4r\xE4k\xE4sittely\xE4\
  , mutta voimme k\xE4ytt\xE4\xE4 kirjastoja kuten `TimeLib.h`. T\xE4ss\xE4 esimerkki\
  \ tulevan p\xE4iv\xE4m\xE4\xE4r\xE4n laskennasta."
lastmod: '2024-03-13T22:44:56.836954-06:00'
model: gpt-4-1106-preview
summary: "Arduino ei suoraan tue p\xE4iv\xE4m\xE4\xE4r\xE4k\xE4sittely\xE4, mutta\
  \ voimme k\xE4ytt\xE4\xE4 kirjastoja kuten `TimeLib.h`."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## How to:
Arduino ei suoraan tue päivämääräkäsittelyä, mutta voimme käyttää kirjastoja kuten `TimeLib.h`. Tässä esimerkki tulevan päivämäärän laskennasta:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(17, 30, 0, 8, 3, 2023); // Asetetaan nykyhetki (hh, mm, ss, pp, kk, vvvv)
}

void loop() {
  time_t nykyhetki = now();
  time_t tulevaisuus = nykyhetki + SECS_PER_DAY * 7; // Lisätään 7 päivää

  Serial.print(day(tulevaisuus));
  Serial.print(".");
  Serial.print(month(tulevaisuus));
  Serial.print(".");
  Serial.println(year(tulevaisuus));

  delay(30000); // Odotetaan 30 sekuntia ennen seuraavaa laskentaa
}
```

## Deep Dive
Arduinon maailmassa ajankäsittely perustuu `millis()`-funktioon, joka laskee millisekunteja laitteen käynnistyksestä. Historiallisesti mikrokontrollerit eivät ole kelloneet päivämääriä mutta lisäkirjastot, kuten `TimeLib`, ovat tulleet avuksi.

Vaihtoehtoisesti RTC (Real-Time Clock) moduuleja voidaan käyttää tarkempiin aikaleimoihin ja ne säilyttävät ajan jopa virrankatkaisun aikana.

Päivämäärälaskennassa kannattaa huomioida karkausvuodet ja kuukausien eri pituudet. `TimeLib`-kirjasto hoitaa nämä yksityiskohdat puolestasi.

## See Also
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Arduino TimeAlarms Library](https://www.pjrc.com/teensy/td_libs_TimeAlarms.html)
- [Adafruit RTClib](https://github.com/adafruit/RTClib)
