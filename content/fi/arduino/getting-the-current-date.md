---
title:                "Arduino: Nykyisen päivämäärän saaminen"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Arduino on monipuolinen ja suosittu ohjelmointialusta, joka tarjoaa mahdollisuuksia monenlaisiin projekteihin. Yksi tärkeä osa Arduino-ohjelmointia on päästä käsiksi nykyiseen päivämäärään ja kellonaikaan. Tässä blogikirjoituksessa käymme läpi, miten voit saada Arduino-laitteen näyttämään nykyisen päivämäärän ja kellonajan.

## Miten

Saadaksesi nykyisen päivämäärän ja kellonajan Arduino-laitteelle, sinun tulee ensin asettaa RTC (Real-Time Clock) moduuli. RTC-moduulit ovat pieniä piirejä, jotka pitävät sisällään tarkan kellon, jota voidaan käyttää nykyisen päivämäärän ja kellonajan näyttämiseen. Useimmissa tapauksissa RTC-moduuleja käytetään ulkoisena laitteena, joka liitetään Arduinoon.

Alla olevassa koodiesimerkissä näytetään, miten voit asettaa RTC-moduulin ja saada sieltä nykyisen päivämäärän Arduino-laitteelle:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();

  // Asetetaan RTC-moduuli nykyiseen päivämäärään ja kellonaikaan
  // Tässä esimerkissä asetetaan 7. maaliskuuta 2021 kello 12.00
  rtc.adjust(DateTime(2021, 3, 7, 12, 0, 0));
}

void loop() {
  // Luodaan DateTime -muuttuja ja tallennetaan siihen nykyinen aika RTC-moduulista
  DateTime now = rtc.now();

  // Tulostetaan päivämäärä ja kellonaika sarjamonitoriin
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();

  delay(1000);
}
```

Kun koodi on ladattu ja Arduino on kytketty RTC-moduuliin, sarjamonitori näyttää nykyisen päivämäärän ja kellonajan. Käyttämällä tätä tietoa voit esimerkiksi ohjelmoida Arduino-laitteen suorittamaan tiettyjä toimintoja tiettyinä aikoina päivästä.

## Syvällinen sukellus

RTC-moduuleilla on usein omat akut, jotka pitävät kellon käynnissä, vaikka Arduino laitetaan pois päältä. Tämä tarkoittaa, että RTC-moduuli pysyy ajan tasalla myös silloin, kun Arduino ei ole kytkettynä virtalähteeseen.

On kuitenkin tärkeää huomata, että RTC-moduulien tarkkuus voi vaihdella ja ajan kulumista on syytä tarkkailla ja tarvittaessa korjata käyttämällä esimerkiksi Internetin aikapalveluja.

## Katso myös

- RTCmoduulin tarkka asettaminen: https://forum.arduino.cc/index.php?topic=126397.0
- RTCmoduulin käyttöohje: https://www.arduino.cc/en/Reference/RTC
- RealTime PCLibrary: https://github.com/adafruit/RTClib