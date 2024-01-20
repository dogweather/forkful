---
title:                "Merkkijonon kirjainten muuttaminen isoiksi"
html_title:           "Arduino: Merkkijonon kirjainten muuttaminen isoiksi"
simple_title:         "Merkkijonon kirjainten muuttaminen isoiksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen isoiksi kirjoittaminen on prosessi, jossa kaikki merkkijonokirjeet muunnetaan isoihin kirjaimiin. Ohjelmoijat tekevät sen tehdäkseen merkkijonoista huomattavampia tai yhdenmukaistavat tiedon ulkoasun.

## Kuinka Tehdä:

Tässä on perusesimerkki siitä, kuinka muunnat merkkijonon isoiksi kirjaimiksi Arduino-ympäristössä:
```Arduino
void setup() {
  // Sarjaportin määrittäminen kommunikointia varten.
  Serial.begin(9600);
}

void loop() {
  
  String s = "ohjelmointia suomeksi";
  s.toUpperCase();
  
  Serial.println(s);
  delay(2000);
  
}
```
Kun tämä koodi toteutetaan, tulostus on seuraava:
```Arduino
OHJELMOINTIA SUOMEKSI
```

## Sukellus syvyyksiin:

Tämä merkkijonojen pääoman vaihto ei ole uusi käsite. Se on ollut mukana ohjelmointikielessä jo pitkään, ja sitä käytetään usein, kun haluat tehdä tekstistäsi sääntöjen mukaista tai kun haluat tehdä tekstin erottuvammaksi.

On olemassa vaihtoehtoinen tapa isontaa merkkijonoja tikutusfunktion avulla. Se on yksityiskohtaisempi ja monimutkaisempi lähestymistapa, mutta se tarjoaa enemmän joustavuutta. Tämä tapa on kuitenkin harvinaisempi, koska useimmissa tapauksissa `toUpperCase` -metodi on riittävä.

On tärkeää myös ymmärtää, että `toUpperCase`-metodi muuttaa alkuperäisen merkkijonon. Jos haluat säilyttää alkuperäisen merkkijonon samalla kun luot iso kirjain version, sinun on ensin kopioitava merkkijono.

## Katso Myös:

Jos haluat oppia lisää merkkijonojen muuntamisesta, tässä on muutamia linkkejä, jotka voivat olla hyödyllisiä:
3. Arduino Programming Course: [Link](https://startingelectronics.org/software/arduino/learn-to-program-course/)