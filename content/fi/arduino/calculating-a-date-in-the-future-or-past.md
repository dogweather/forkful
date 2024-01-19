---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Arduino: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä: Miksi?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä tarkoittaa aikaleimajon notaation muuttamista vastaamaan haluttua aikapistettä. Ohjelmoijat tekevät niin sovellusten logiikan tarpeiden, kuten tapahtumien ajastuksen tai ajanhallinnan vuoksi.

# Näin se tehdään:

Aloita asettamalla RTC-moduuli (Real Time Clock) ja DD-MM-YYYY muodossa oleva päivämäärä. Lasketaan sitten päiviä tulevaisuudessa tai menneisyydessä.

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
 Wire.begin();
 rtc.begin();
 
 if (! rtc.isrunning()) {
   rtc.adjust(DateTime(__DATE__, __TIME__));
 }

 DateTime aika_nyt = rtc.now();
 DateTime tulevaisuuden_paiva = aika_nyt + TimeSpan(7,0,0,0); // lasketaan viikko (7 päivää) eteenpäin
}

void loop () {
 DateTime aika_nyt = rtc.now();
       
 Serial.print("Aika nyt: ");
 Serial.println(aika_nyt);
 delay(1000);
 
 if (aika_nyt >= tulevaisuuden_paiva) {
   Serial.println("Viikko on kulunut!");
 }
}
```
Output:
```
Aika nyt: 2022-02-14 12:01:25
...
Viikko on kulunut!
```

# Syvempi sukellus:

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on olennainen osa monia ohjelmasovelluksia. Muinaiset ohjelmoijat joutuivat tekemään tämän käsin, mutta Arduino-RTC-moduulit kuten DS1307 helpottavat nyt tätä prosessia huomattavasti.

Vaihtoehtoisesti voit käyttää aikakirjastoa, joka tarjoaa useita funktioita päivämäärän ja ajan käsittelyyn. Mutta RTC-moduulien käyttö on parempi, sillä se on tarkempi ja se ei nollaudu, kun Arduino käynnistetään uudelleen.

Kun lasket päiviä tulevaisuudessa tai menneisyydessä, muista, että kuukausien päivien määrä vaihtelee ja ottaa huomioon myös karkausvuodet.

# Katso myös:

[Arduino Time Library](https://playground.arduino.cc/Code/Time)  
[RTC Library](https://github.com/adafruit/RTClib)  
[Arduino’s official website](https://www.arduino.cc/)