---
title:    "Arduino: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein Arduino-ohjelmointia tehdessä tulee tarvetta laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä. Tämä voi olla esimerkiksi tarpeellista ajastettujen tapahtumien suunnittelussa tai tietyissä datan tallennuksessa. Seuraavaksi kerromme, miten tämä voidaan toteuttaa Arduino-ohjelmoinnissa.

## Miten

Lasketun päivämäärän saamiseksi tulee ensin määrittää nykyinen päivämäärä. Tämä voidaan tehdä hyödyntämällä RTC (real-time clock) moduulia tai käyttämällä Arduino:n Time Librarya. Esimerkiksi, jos haluamme saada tulevan päivämäärän 10 päivän päästä, voimme käyttää seuraavaa koodinpätkää:

```Arduino
#include <TimeLib.h>

void setup() {
  // Määritetään nykyinen päivämäärä
  setTime(11, 11, 2019, 12, 0, 0);
}

void loop() {
  // Lasketaan päivämäärä 10 päivän päähän
  time_t futureDate = now() + (10L * 24L * 3600L);

  // Tulostetaan laskettu päivämäärä sarjaporttiin
  Serial.print("Tuleva päivämäärä: ");
  Serial.println(day(futureDate));
  Serial.print(".");
  Serial.print(month(futureDate));
  Serial.print(".");
  Serial.println(year(futureDate));

  delay(1000); // Pieni viive seuraavaan laskentakertaan
}
```

Tulostus sarjaporttiin: Tuleva päivämäärä: 21.11.2019

Voimme myös laskea menneisyydessä olevia päivämääriä, muuttamalla laskettavan päivämäärän merkkiä miinukseksi. Esimerkiksi, jos haluamme laskea päivämäärän 3 päivää taaksepäin, voimme käyttää seuraavaa koodinpätkää:

```Arduino
#include <TimeLib.h>

void setup() {
  // Määritetään nykyinen päivämäärä
  setTime(11, 11, 2019, 12, 0, 0);
}

void loop() {
  // Lasketaan päivämäärä 3 päivää taaksepäin
  time_t pastDate = now() + (-3L * 24L * 3600L);

  // Tulostetaan laskettu päivämäärä sarjaporttiin
  Serial.print("Menneisyyden päivämäärä: ");
  Serial.println(day(pastDate));
  Serial.print(".");
  Serial.print(month(pastDate));
  Serial.print(".");
  Serial.println(year(pastDate));

  delay(1000); // Pieni viive seuraavaan laskentakertaan
}
```

Tulostus sarjaporttiin: Menneisyyden päivämäärä: 8.11.2019

## Syvempi tarkastelu

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä perustuu ajanlaskuun ja yksinkertaiseen matemaattiseen laskutoimitukseen. Kun nykyinen päivämäärä on tiedossa, voidaan tuleva tai menneinen päivämäärä laskea lisäämällä tai vähentämällä tietty määrä sekunteja nykyiseen aikaan. Arduino:n Time Libraryssa tämä toteutetaan time_t-tyypillä, joka sisältää nykyisen ajan sekunteina vuodesta 1970.

On myös hyvä huomioida, että päivämäärä lasketaan UNIX-päivinä. 1 UNIX-päivä vastaa 24*60*60 eli 86400 sek