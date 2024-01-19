---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
"Substringin" poimiminen tarkoittaa merkkijonon osan erottamista. Ohjelmoijat tekevät sen tarvitessaan tietystä merkkijonosta vain tiettyä osaa, esimerkiksi nimeä tai osoitetta.

## Kuinka Tehdään:
Seuraavassa on esimerkki substrings-funktion käytöstä Arduino-koodissa.

```Arduino
String teksti = "Tervetuloa Arduino-ohjelmoinnin maailmaan!";
String osa = teksti.substring(12, 21);
Serial.println(osa);  //tulostaa "Arduino"
```
Tämä koodi luo merkkijonon "teksti", ottaa siitä osan indeksivälillä 12-21 ja tulostaa sen.

## Etene Syvemmälle:
Substring-toiminnon esittelemistä aiemmin ohjelmoijat usein luopuivat sivuttaessaan pitkiä merkkijonoja manuaalisesti, mikä oli aikaa vievää ja virhealtista. Tämän funktion avulla voit keskittyä merkkijonon tärkeimpään osaan automaattisesti.

Vaihtoehtoisesti, voit taistella toisen merkkijono-funktion, "indexOf()", kanssa. Tämä antaa sinulle ensimmäisen esiintymän sijainnin merkkijonossa, jonka voit sitten syöttää "substring()" -funktioon.

Toteutustietojen osalta Arduino leikkaa merkkijonot "laiskasti". Tämä tarkoittaa, että se tekee todellisen leikkauksen vain, kun tarvitset puristetun merkkijonon. Tämä säästää luonnollisesti arvokasta aikaa.

## Katso Lisää:
Lisätietoja löydät seuraavista lähteistä:
- Arduino ohjelmistokehitys : https://www.arduino.cc/
- String luokan dokumentaatio : https://www.arduino.cc/en/Reference/StringObject
- Tehokas ohjelmointi Arduino: https://learn.adafruit.com/adafruit-arduino-lesson-11-lcd-displays-1/arduino-sketch