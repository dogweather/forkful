---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Nykyisen päivämäärän hankkiminen tarkoittaa päivämäärän ja ajan lukemista reaaliajassa. Ohjelmoijat tekevät tämän kerätäkseen ajantasaisia tietoja tai suorittaakseen ajastettuja tehtäviä.

## Miten toimia:

Käytämme Arduino Uno R3 -laitetta ja siihen liitettyä DS3231 RTC -moduulia. Tässä on esimerkkikoodi, jolla saadaan nykyinen päivämäärä:

```Arduino
#include <DS3231.h>

DS3231  rtc(SDA, SCL);

void setup()
{
  rtc.begin();
}

void loop()
{
  Serial.print("Päivämäärä: ");
  Serial.print(rtc.getDateStr());
  Serial.print("\n");
  delay(1000);
}
```

Koodi tulostaa nykyisen päivämäärän joka sekunti.

## Tarkempi tarkastelu:

DS3231 on RTC (Real Time Clock) -moduuli, joka perustuu I2C-protokollaan. Se ei ainoastaan pysty tallentamaan sekunteja, minuutteja, tunteja, päivää, kuukautta ja vuotta, vaan siinä on myös lämpötila-anturi.

Arduino Uno R3:ssa ei ole sisäänrakennettua RTC:ää, siksi tarvitsemme ulkoisen RTC-moduulin. Historiallisesti tietokoneissa on pitkään käytetty CMOS RTC:tä.

Vaihtoehtona on käyttää GPS-moduulia. Se toimii hyvänä ajanlähteenä, mutta vaatii selkeän näköyhteyden satelliitteihin. Verkkopalvelut, kuten NTP (Network Time Protocol), voivat tarjota tarkan ajan Internetin kautta, vaikkakin verkkoyhteys vaaditaan.

DS3231-kirjaston `getDateStr()`-funktio antaa oikeanmuotoisen päivämäärän (muodossa "DD.MM.YYYY").

## Katso myös:

2. Miten käyttää NTP:tä ajan saamiseksi Arduinossa [Random Nerd Tutorials](https://randomnerdtutorials.com/esp8266-nodemcu-date-time-ntp-client-server-arduino/).