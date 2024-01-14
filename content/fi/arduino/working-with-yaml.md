---
title:                "Arduino: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi YAML on hyödyllinen Arduino-ohjelmoinnissa?

YAML (YAML Ain't Markup Language) on yksi suosituimmista tiedon esityskielistä, jota käytetään useissa erilaisissa ohjelmointikielissä, kuten myös Arduinossa. Se on yksinkertainen ja helposti luettavissa oleva tapa tallentaa tietoa, mikä tekee siitä erityisen hyödyllisen Arduino-projekteissa. YAML:n avulla voit tallentaa tietoa rakenteellisessa muodossa, joka on helppo lukea ja käsitellä ohjelmassa.

## Kuinka käyttää YAML:ää Arduinossa?

Aloittaaksesi YAML:n käytön Arduinossa, sinun tulee ensin asentaa YAML-kirjasto käyttämäsi Arduino IDE:n kautta. Voit tehdä tämän menemällä "Työkalut" valikkoon ja valitsemalla "Hallitse kirjastoja". Etsi hakukentästä "YAML" ja asenna "YAML" kirjasto, joka näkyy tuloksissa.

Seuraavaksi sinun tulee liittää kirjasto ohjelmaasi ja luoda uusi YAML-objekti, jonka avulla voit käsitellä YAML-dataa. Tässä on yksinkertainen esimerkki, joka tallentaa ja tulostaa muutaman tiedon YAML-muodossa:

```Arduino
#include <YAML.h>

YAML yaml;

String nimi = "Matti";
int ikä = 30;
float paino = 75.5;

void setup() {
  // Liitetään YAML-kirjasto
  yaml.begin();
  
  // Luodaan YAML-objekti
  YAML::Node tiedot = yaml.createNode();
  
  // Lisätään tietoja YAML-objektiin
  tiedot["nimi"] = nimi;
  tiedot["ikä"] = ikä;
  tiedot["paino"] = paino;
  
  // Tulostetaan YAML-data sarjaliikenteeseen
  String sarjaliikenne = yaml.write(tiedot);
  Serial.println(sarjaliikenne);
}

void loop() {
  // Tyhjä looppi
}
```

Tämän esimerkin output sarjaliikenteessä tulostaisi seuraavan:

```
nimi: Matti
ikä: 30
paino: 75.5
```

Voit myös tallentaa ja lukea YAML-dataa tiedostosta käyttäen YAML-kirjaston `load()` ja `save()` funktioita. Hyvänä lisänä on myös käyttää `Serial Monitor`ia tutkimaan ja vahvistamaan dataa, johon olet tallentanut YAML-muodossa.

## Syvällisempää tietoa YAML:stä

Vaikka YAML on helppo ja yksinkertainen käyttää Arduinossa, se voi olla hieman monimutkaisempaa esimerkiksi silloin, kun tallennat monimutkaisempaa dataa, kuten taulukoita tai objekteja. Tässä tapauksessa on hyvä tutustua tarkemmin YAML:n syntaksiin ja käyttötapoihin. Voit aloittaa lukemalla YAML:n virallista dokumentaatiota (https://yaml.org/spec/1.2/spec.html) ja kokeilemalla lisää esimerkkejä.

## Katso myös

- YAML-kirjaston dokumentaatio (https://arduinojson.org/doc/jsonobject/)
- Arduinon viralliset verkkosivut (https://www.arduino.cc/)
- Ohjeita aloittelijoille Arduinossa (https://www.arduino.cc/en/Guide/Introduction)