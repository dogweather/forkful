---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Arduino: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa projekteissa on tarvetta muuntaa päivämäärä nimenomaiseksi merkkijonoksi, jotta voidaan näyttää päivämäärä tarkemmin tai tallentaa se tiedostoon. Arduino-pohjaiset projektit eivät ole poikkeus. Tässä artikkelissa opit, miten voit helposti muuntaa päivämäärän merkkijonoksi käyttäen Arduino-ohjelmointikieltä.

## Miten

Muuntaaksesi päivämäärän merkkijonoksi Arduinolla, tarvitset kaksi kirjastoa: `RTClib` ja `Wire`. Ensimmäinen kirjasto vastaa oikean ajan saamisesta ja toinen kirjasto on vastuussa kommunikaatiosta RTC-piirin kanssa. Tallenna ensin `RTClib.h` tiedosto ja `Wire.h` kirjasto kansioosi ja sisällytä ne koodiisi seuraavasti:

```Arduino
#include <Wire.h>
#include <RTClib.h>
```

Sitten sinun on luotava RTC-olio ja käynnistettävä RTC-kirjasto:

```Arduino
RTC_DS3231 rtc;
```

Oletetaan, että haluat muuntaa ja tulostaa nykyisen päivämäärän sarjaporttiin. Ensimmäinen vaihe on aloittaa sarjaportin käyttö:

```Arduino
Serial.begin(9600);
```

Sitten sinun on avattava RTC-yhteys `begin()` -toiminnon avulla:

```Arduino
rtc.begin();
```

Viimeinen askel on käyttää `now()` -toimintoa saadaksesi nykyisen päivämäärän ja tallentaa se muuttujaan. Sitten voit helposti muuntaa päivämäärän merkkijonoksi ja tulostaa sen sarjaporttiin:

```Arduino
DateTime now = rtc.now(); // saa nykyisen päivämäärän
String date = now.timestamp(DateTime::TIMESTAMP_DATE); // muuntaa päivämäärän merkkijonoksi
Serial.println(date); // tulostaa merkkijonon sarjaporttiin
```

Jos kaikki onnistui, tulosteena pitäisi olla nykyinen päivämäärä muodossa `dd.mm.yyyy`. Voit myös muuttaa päivämäärän muotoa vaihtamalla `DateTime::TIMESTAMP_DATE` vaihtoehtoa. Voit esimerkiksi käyttää `DateTime::TIMESTAMP_FULL` saadaksesi tulosteen muodossa `hh:mm:ss dd.mm.yyyy`.

## Deep Dive

Jos haluat syventää ymmärrystäsi päivämäärän muuntamisesta merkkijonoksi, voit tutustua RTC-kirjaston lähdekoodiin. Sieltä löytyy tarkempi dokumentaatio, joka auttaa sinua ymmärtämään paremmin eri toimintoja ja vaihtoehtoja.

Voit myös kokeilla vaihtoehtoisia tapoja muuntaa päivämäärä merkkijonoksi, kuten käyttämällä `char` -muuttujaa tai `sprintf()` -toimintoa. Näiden vaihtoehtojen avulla voit tuottaa erilaisia merkkijonotulosteita ja sovittaa ne paremmin juuri sinun projektiisi.

## Katso myös

- [Arduino Reference – RTC DS3231](https://www.arduino.cc/reference/en/libraries/rtc/)
- [RTC Piirilevy Arduinolle](https://www.sparkfun.com/products/12708)
- [Eri muotoisia päivämääränäkymät Arduinolle](https://www.pjrc.com/teensy/td_libs_Time.html)