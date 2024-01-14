---
title:    "Arduino: Nykyisen päivämäärän hankkiminen."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat saada nykyisen päivämäärän Arduinoa ohjelmoitaessa? On monia syitä, miksi tämä voi olla tärkeää, kuten esimerkiksi aikaleimojen tallentaminen datalle tai aikapohjaisten tehtävien suorittaminen.

## Kuinka

Voit helposti saada nykyisen päivämäärän Arduino-ohjelmassa käyttämällä `millis()` -funktiota, joka palauttaa ajan millisekunteina Arduino-laitteen käynnistysajan jälkeen. Tästä voidaan laskea nykyinen päivämäärä ja aika käyttämällä `millis()` -arvoa yhdessä `time()` tai `since()` -funktioiden kanssa käyttäen sopivaa kaavaa. Katso alla oleva esimerkki:

```Arduino
unsigned long aika = millis(); // tallennetaan käynnistysaika muuttujaan
unsigned long paivat = aika / (1000 * 60 * 60 * 24); // päivien määrä käynnistyksestä
unsigned long tunnit = (aika / (1000 * 60 * 60)) % 24; // tunnit käynnistyksestä
unsigned long minuutit = (aika / (1000 * 60)) % 60; // minuutit käynnistyksestä
unsigned long sekunnit = (aika / 1000) % 60; // sekunnit käynnistyksestä
```

Tämä esimerkki palauttaa nykyisen päivämäärän muuttujiin `paivat`, `tunnit`, `minuutit` ja `sekunnit`, jotka voidaan sitten tulostaa näytölle tai käyttää muilla tavoin haluamallasi tavalla.

## Syvällisempi tarkastelu

Yllä oleva esimerkki antaa vain yksinkertaisen tavan saada nykyinen päivämäärä Arduino-ohjelmassa. On kuitenkin tärkeää huomata, että `millis()` ei palauta todellista kelloaikaa, vaan ajanjakson, joka on kulunut Arduino-laitteen käynnistyksestä. Joten jos laite käynnistetään uudelleen, nykyinen päivämäärä ja aika nollaantuvat.

Jotta voitaisiin saada todellinen nykyinen päivämäärä ja aika, sinun on käytettävä ulkoista RTC-moduulia (Real-Time Clock). RTC-moduuli sisältää oman kellonsa ja akkunsa, joka jatkaa toimintaansa myös, kun laite kytketään pois päältä tai käynnistetään uudelleen.

Voit lukea lisää RTC-moduuleista ja niiden käytöstä Arduino-projekteissa täältä: [Tutorial: Using DS1307 and DS3231 Real-time Clock Modules with Arduino](https://www.circuitbasics.com/arduino-ds1307-real-time-clock-tutorial/)

## Katso myös

- [millis() - Arduino Reference](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Time Library - Arduino Libraries](https://www.arduino.cc/en/Reference/Time)
- [DS1307 Real-time Clock Module - Adafruit](https://www.adafruit.com/product/264)