---
title:                "Tulevan tai menneen päivämäärän laskeminen"
aliases:
- /fi/arduino/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:03.329616-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

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
