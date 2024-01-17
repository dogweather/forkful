---
title:                "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
html_title:           "Arduino: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Laskeminen tulevaan tai menneeseen päivämäärään on ohjelmoinnin osa, joka mahdollistaa ohjelmoijien luoda monipuolisia ja ajankohtaisia sovelluksia. Laskennan avulla voit esimerkiksi tarkistaa tulevan tapahtuman ajankohdan tai näyttää erilaisia aikaperusteisia ilmoituksia.

## Kuinka?
Laskeminen tulevaan tai menneeseen päivämäärään tapahtuu usein käyttämällä aikaperusteista funktiota. Alla on esimerkki koodista, joka laskee 10 päivää eteenpäin nykyisestä päivämäärästä ja tulostaa uuden päivämäärän.

```
Arduino-ohjelmassa voit käyttää seuraavaa koodia:

  // importataan aikaperusteinen kirjasto
  #include <Time.h>

  // asetetaan nykyinen päivämäärä ja aika
  setTime(12,00,00,2,11,2020);
  
  // lasketaan 10 päivää lisää nykyiseen päivämäärään ja tulostetaan uusi päivämäärä
  time_t futureDate = now() + (10 * 24 * 60 * 60); // 10 päivää = 10 * 24 tuntia * 60 minuuttia * 60 sekuntia
  Serial.println(day(futureDate));
  Serial.println(month(futureDate));
  Serial.println(year(futureDate));

Tämä koodi tuottaa seuraavan tulosteen:

22
2
2020
```

## Syvemmälle
Aikaperusteinen laskenta on ollut osa ohjelmointia jo vuosikymmeniä. Alkuaikoina se oli yksinkertainen tapa tarkistaa nykyinen aika ja päivämäärä, mutta nykyään sitä käytetään monipuolisemmin esimerkiksi tapahtumien ajastamiseen tai erilaisten ajankohtaisten tietojen näyttämiseen.

Laskentaan on myös muita tapoja, kuten käyttämällä kellopiiriä tai Internet-yhteyttä, mutta aikaperusteinen laskenta on edelleen yksi yleisimmin käytetyistä tavoista, koska se ei vaadi lisälaitteita tai yhteyksiä.

Aikaperusteinen laskenta voidaan toteuttaa myös erilaisilla ohjelmointikielillä ja sovellusympäristöillä, mutta Arduino tarjoaa helpon tavan aloittelijoille lähteä kokeilemaan.

## Katso myös
- [Arduino Time Library](http://playground.arduino.cc/Code/time)
- [Arduino Playground](http://playground.arduino.cc/)