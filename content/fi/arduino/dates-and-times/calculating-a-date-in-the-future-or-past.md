---
date: 2024-01-20 17:31:03.329616-07:00
description: "Mit\xE4 on p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen tulevaisuuteen tai\
  \ menneisyyteen? K\xE4yt\xE4nn\xF6ss\xE4 s\xE4\xE4d\xE4mme kelloja ja kalentereita\
  \ ohjelmallisesti. Miksi? Kalenteritoiminnot\u2026"
lastmod: '2024-03-13T22:44:56.836954-06:00'
model: gpt-4-1106-preview
summary: "Mit\xE4 on p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen tulevaisuuteen tai menneisyyteen?\
  \ K\xE4yt\xE4nn\xF6ss\xE4 s\xE4\xE4d\xE4mme kelloja ja kalentereita ohjelmallisesti.\
  \ Miksi? Kalenteritoiminnot\u2026"
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## What & Why?
Mitä on päivämäärän laskeminen tulevaisuuteen tai menneisyyteen? Käytännössä säädämme kelloja ja kalentereita ohjelmallisesti. Miksi? Kalenteritoiminnot ovat kriittisiä muistutuksille, aikatauluille ja tapahtumien seurannalle.

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
