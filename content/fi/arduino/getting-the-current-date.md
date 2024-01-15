---
title:                "Päivämäärän saaminen"
html_title:           "Arduino: Päivämäärän saaminen"
simple_title:         "Päivämäärän saaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko tietää, mikä päivä on tänään? Arduinoilla voit helposti hakea nykyisen päivämäärän ja käyttää sitä esimerkiksi projekteissasi tai näytöllä näytettävissä tiedoissa.

## Miten

```Arduino
#include <TimeLib.h>

void setup() {
  // Alustetaan sarjaportti
  Serial.begin(9600);
  // Alustetaan ajanhallinta
  setTime(12, 35, 0, 10, 6, 2021); // tunti, minuutit, sekunnit, päivä, kuukausi, vuosi
}

void loop() {
  // Haetaan nykyinen aika
  int hour = hour();
  int minute = minute();
  int second = second();
  // Haetaan nykyinen päivä
  int day = day();
  int month = month();
  int year = year();
  // Tulostetaan sarjaporttiin
  Serial.print("Tänään on ");
  Serial.print(day);
  Serial.print(".");
  Serial.print(month);
  Serial.print(".");
  Serial.print(year);
  Serial.print(" ja kello on ");
  Serial.print(hour);
  Serial.print(":");
  Serial.print(minute);
  Serial.print(":");
  Serial.println(second);
  // Pysäytetään ohjelma
  while (true);
}
```

Tulostus:

```
Tänään on 10.6.2021 ja kello on 12:35:00
```

## Syvemmälle

Mikäli haluat hyödyntää ajanhallintaa tarkemmin, voit tutustua Arduinon TimeLib-kirjastoon. Kirjaston avulla voit esimerkiksi asettaa tarkan aloitusajan ja laskea kulunutta aikaa. Voit myös hyödyntää Arduinon ulkoista kellopiiriä, kuten RTC-moduulia, mikäli haluat tarkemman ajanhallinnan.

## Katso myös

- [Arduino TimeLib kirjasto](https://github.com/PaulStoffregen/Time)
- [RTC-moduuli Arduinolle](https://maker.pro/arduino/tutorial/how-to-use-an-rtc-module-to-make-a-simple-arduino-clock)