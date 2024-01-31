---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:35:16.666305-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärän lukemista ja ymmärtämistä tekstiformaatista. Ohjelmoijat tarvitsevat tätä toimintoa, koska päivämääriä tallennetaan ja siirretään usein tekstimuodossa ja ne täytyy muuttaa käsiteltävään muotoon.

## How to: - Kuinka:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC ei löydy!");
    while (1);
  }
  rtc.adjust(DateTime(__DATE__, __TIME__));
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Päivämäärä: ");
  Serial.print(now.day());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.println(now.year());

  delay(1000);
}
```
Tulostus:
```
Päivämäärä: 7/12/2023
```

## Deep Dive - Syväluotaus:
Päivämäärän jäsentäminen merkkijonosta on yleinen tehtävä ohjelmoinnissa. Historiallisesti lukuisia kirjastoja ja funktioita on kehitetty tämän toiminnallisuuden tarjoamiseksi. Arduinossa käytetään usein `RTClib`-kirjastoa yhdessä reaaliaikakellomoduulin kanssa, kuten osoitetaan esimerkissä. Vaihtoehtoja kuten `strftime`- ja `strptime`-funktiot ovat suosittuja C-kielessä, mutta Arduino-kirjastoissa ne ovat harvemmin tuettuja.

Jäsentämisen toteutuksessa keskeistä on ymmärtää käytetty päivämääräformaatti. Eri alueet käyttävät eri formaatteja (esim. kuukausi/päivä/vuosi tai päivä/kuukausi/vuosi), joten ohjelmoijan on tunnettava konteksti johon sovellus on tarkoitettu. Arduinon tapauksessa olemme usein vuorovaikutuksessa laitteiston, kuten RTC-moduulin, kanssa, mikä poikkeaa puhtaasti ohjelmistopohjaisista ratkaisuista.

## See Also - Katso Myös:
- RTClib-kirjasto: https://github.com/adafruit/RTClib
- Arduino DateString nimiavaruuden dokumentaatio: https://www.arduino.cc/reference/en/libraries/rtclib/datetime/
- Arduinon virallinen Time-kirjasto: https://www.arduino.cc/en/Reference/Time
