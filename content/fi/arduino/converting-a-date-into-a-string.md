---
title:                "Arduino: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

## Miksi sinun kannattaisi muuttaa päivämäärä merkkijonoksi Arduino-ohjelmoinnissa? Tässä tulee muutama hyvä syy.

Ehkä haluat näyttää päivämäärän Arduino-projektissasi käyttäjille tai tallentaa sen johonkin tärkeään tietokantatietueeseen. Tai ehkä haluat vain oppia uutta koodausta ja haastaa itseäsi. Riippumatta syystäsi, oppimalla päivämäärän muuntamisen merkkijonoksi, voit lisätä monipuolisuutta ja joustavuutta Arduino-projekteihisi.

## Kuinka

## Tässä on yksinkertainen esimerkki siitä, kuinka muuttaa päivämäärä merkkijonoksi Arduino-ohjelmassa:

```Arduino
#include <RTClib.h> // Lisää RTC (Real Time Clock) -kirjasto

RTC_DS1307 rtc; // Luo RTC-olio

void setup() {
  Serial.begin(9600); // Käynnistä sarjaportti
  rtc.begin(); // Käynnistä RTC
}

void loop() {
  DateTime now = rtc.now(); // Lue nykyinen päivämäärä ja aika RTC:ltä
  // Muunna päivämäärä merkkijonoksi ja tulosta se sarjaportilla
  Serial.print(now.day(), DEC);
  Serial.print("/");
  Serial.print(now.month(), DEC);
  Serial.print("/");
  Serial.print(now.year(), DEC);
  delay(1000); // Odota sekunti ennen uuden päivämäärän lukemista
}
```

Esimerkissä käytämme RTClib-kirjastoa, joka helpottaa RTC:n käyttöä Arduino-ohjelmoinnissa. Muunnumme sitten päivän, kuukauden ja vuoden arvoiksi ja tulostamme ne sarjaportilla. Näin saat päivämäärän muunnettuna merkkijonoksi "DD/MM/YYYY" -muodossa.

Toinen tapa muuttaa päivämäärä merkkijonoksi on käyttää `sprintf()` -funktiota, joka on C-kielessä käytetty merkkijonon muotoilufunktio. Esimerkiksi seuraava koodi antaa saman tuloksen kuin edellinen koodin pätkä:

```Arduino
DateTime now = rtc.now();

char dateStr[11]; // Luodaan merkkijono, johon päivämäärä tallennetaan
sprintf(dateStr, "%02d/%02d/%04d", now.day(), now.month(), now.year());
Serial.println(dateStr); // Tulostaa "DD/MM/YYYY"
```

## Syvempi sukellus

Päivämäärän muuttaminen merkkijonoksi voi olla hyödyllistä monessa eri tilanteessa. Kirjoittaessasi muotoilumerkkijonoa (`"%02d/%02d/%04d"`), päivämäärän osat (päivä, kuukausi, vuosi) on eroteltu prosenttimerkillä (`%`) ja kirjaimella, joka ilmaisee datatyyppiä (`d` tarkoittaa kokonaislukua, `02` ja `04` tarkoittaa, että luvut täydennetään nollilla tarvittaessa).

Lisäksi `sprintf()`-funktiossa on muitakin käteviä merkintöjä, kuten `%02X`, joka tulostaa heksadesimaalilukunasi (`X` tarkoittaa heksakoodia). Voit löytää lisätietoa `sprintf()`-funktiosta ja sen muotoilumerkinnöistä esimerkiksi Arduinon dokumentaatiosta.

## Katso myös

- [Arduino-ohjelmointi aloittelijoille](https://www.arduino.cc/en/Guide/ArduinoUno)
- [RTC-kirjasto](https://github.com/adafruit/RTClib)
- [C: sprintf()-funktio](