---
title:                "Arduino: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi: Miksi haluaisit saada nykyisen päivämäärän Arduinossa?

Jokaisessa projektissa, jossa käytetään Arduinoa, on tärkeää tietää nykyinen päivämäärä ja aika. Tämä voi olla hyödyllistä esimerkiksi tiedostojen tallentamisessa, tapahtumien ajoittamisessa tai mittauksien aikaleimoissa. Tässä blogikirjoituksessa käsittelemme, miten voit saada nykyisen päivämäärän Arduinossa ja miten voit käyttää sitä projekteissasi.

## Kuinka: Näin saat nykyisen päivämäärän Arduinossa

Nykyisen päivämäärän saamiseksi Arduinossa tarvitsemme RTC (Real-Time Clock) -moduulin, joka pitää sisällään kellon sekä kalenterin. Tässä esimerkissä käytämme DS3231 RTC -moduulia, mutta voit käyttää myös muita RTC-malleja, kunhan varmistat, että ne ovat yhteensopivia Arduinon kanssa.

Ensin liitä RTC-moduulin vinottaiset nastat VCC, GND, SDA ja SCL vastaavasti Arduinon 5V, GND, A4 ja A5 nastoihin. Tämän jälkeen voit käyttää tähän blogikirjoitukseen liitettyjä esimerkkikoodeja saadaksesi nykyisen päivämäärän näkyviin Serial monitorissa.

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup () {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();
}

void printCurrentDate() {
  DateTime now = rtc.now();
  Serial.print(now.day());
  Serial.print('.');
  Serial.print(now.month());
  Serial.print('.');
  Serial.print(now.year());
}

void loop () {
  printCurrentDate();
  delay(1000);
}
```

Esimerkkikoodi käyttää RTClib-kirjastoa, joka helpottaa RTC-moduulin käyttämistä Arduinossa. Koodissa luodaan instanssi rtc ja käynnistetään se setup-funktiossa. Loop-funktiossa tulostetaan nykyinen päivämäärä Serial monitoriin. Runsaalla delay-funktioilla voit säätää, kuinka usein nykyinen päivämäärä tulostetaan.

## Syventävä sukellus: Lisätietoja nykyisen päivämäärän saamisesta

Nykyisen päivämäärän saamisessa käytetään RTC-moduulia, joka tarjoaa tarkemman kellon ja kalenterin kuin Arduinon sisäinen kellomoduuli. RTC-moduulissa on oma virtalähteensä, joten se jatkaa toimintaansa myös, kun Arduinon virran irroittaa.

RTClib-kirjaston lisäksi on olemassa muita kirjastoja, kuten TimeLib ja RTCZero, joilla nykyisen päivämäärän saaminen onnistuu. Nämä kirjastot tarjoavat erilaisia toimintoja ja voit valita niistä sinulle sopivan vaihtoehdon.

# Katso myös

- [DS3231 RTC-moduulin käyttö Arduinossa](https://www.instructables.com/id/Arduino-RTC-DS3231/)

- [RTClib-kirjaston esimerkkikoodit](https://github.com/adafruit/RTClib)

- [TimeLib-kirjaston dokumentaatio](https://github.com/PaulStoffregen/Time)

- [RTCZero-kirjaston esimerkkikoodit](https://www.arduino.cc/en/Reference/RTCZero)

*Katri Saarela*