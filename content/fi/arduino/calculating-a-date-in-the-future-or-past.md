---
title:                "Arduino: Laskeminen tulevaisuuden tai menneen päivämäärän kanssa"
simple_title:         "Laskeminen tulevaisuuden tai menneen päivämäärän kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Miksi:
Miksi joku haluaisi laskea tulevan tai menneen päivän Arduino-ohjelmassa? Tässä blogipostauksessa selitämme, kuinka tämä voi olla hyödyllistä ja miten se voidaan tehdä.

# Kuinka:

Laskeminen tulevan tai menneen päivän Arduino-ohjelmassa onnistuu käyttämällä mktime()-funktiota. Tämä funktio ottaa parametreina vuoden, kuukauden ja päivän ja palauttaa aikaleiman sekunneissa. Tämän aikaleiman avulla voimme laskea tietyn päivän esimerkiksi viikon tai kuukauden päästä tai menneisyydessä. Katsotaanpa esimerkkiä:

````Arduino
#include <Time.h> // lisätään aikakirjasto

void setup() {
  Serial.begin(9600); // Alustetaan sarjaportti
}

void loop() {
  // Lasketaan päiväksi 23.7.2021
  time_t t = mktime(year=2021, month=7, day=23, hour=0, min=0, sec=0);

  // Lasketaan päiväksi 4 viikkoa myöhemmin
  t += 4 * 7 * 24 * 3600;

  // Muunnetaan aikaleima takaisin päivämääräksi
  tmElements_t futureDate = breakTime(t);

  // Tulostetaan tulos sarjaporttiin
  Serial.println("Tuleva päivä: ");
  Serial.print(futureDate.Day); Serial.print(" / ");
  Serial.print(futureDate.Month); Serial.print(" / ");
  Serial.println(futureDate.Year);
}
````

Tässä esimerkissä käytämme mktime()-funktiota laskemaan päivää 4 viikkoa eteenpäin ja tulostamme tulevan päivämäärän sarjaporttiin. Tämä on vain yksinkertainen esimerkki, ja voit käyttää samoja periaatteita laskiessasi tulevaa tai menneen päivän Arduino-ohjelmassa.

# Syvempää sukeltamista:

Kun käytät mktime()-funktiota, on tärkeää huomata, että funktio käyttää alkuperäisenä aikana Arduino-järjestelmän kellon aikaa. Jos haluat käyttää tiettyä aikaa laskemiseen, voit asettaa sen setTime()-funktiolla ennen mktime()-funktion käyttöä. Lisäksi voit lukea lisätietoja mktime()-funktion käytöstä Arduino-käsikirjasta.

# Katso myös:

1. [mktime()-funktion dokumentointi Arduino-käsikirjassa](https://www.arduino.cc/en/Reference/MkTime)
2. [Tietoa ajasta ja päivämääristä Arduino-projekteissa](https://forum.arduino.cc/index.php?topic=578106.0)
3. [TimeLib-kirjasto Arduino-projekteihin](https://salvador.mata.dev/arduino-timelib-documentation/)