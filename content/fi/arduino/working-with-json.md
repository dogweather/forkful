---
title:                "Työskentele jsonin kanssa"
html_title:           "Arduino: Työskentele jsonin kanssa"
simple_title:         "Työskentele jsonin kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON (JavaScript Object Notation) on yleisesti käytetty tiedon esitysmuoto, joka on helppo lukea ja kirjoittaa ihmisille ja koneille. Kun työskentelet Arduino-mikrokontrollerin kanssa, JSON:illa voi olla monia hyödyllisiä sovelluksia, kuten tiedon tallentaminen, verkkoviestinnät ja IoT-projektit.

## Kuinka


JSON-tietojen lukeminen ja kirjoittaminen Arduino-koodissa on melko helppoa käyttämällä liibsiä nimeltä "ArduinoJson". Se voidaan helposti ladata ja lisätä omaan koodiin seuraavalla tavalla:

```Arduino
#include <ArduinoJson.h>
```

Ensimmäinen askel on määritellä JSON-muuttuja ja alustaa se halutulla arvolla:

```Arduino
StaticJsonDocument<200> doc; // luodaan JSON-objekti, joka voi sisältää enintään 200 merkkiä
doc["nimi"] = "Matti"; // asetetaan "nimi"-avaimen arvoksi "Matti"
doc["ikä"] = 30; // asetetaan "ikä"-avaimen arvoksi 30

```

JSON-objektin sisältämät tiedot voidaan myös hakea helposti käyttämällä avaimia:

```Arduino
int ika = doc["ikä"];
// ika-muuttujaan tallennetaan arvo 30

String nimi = doc["nimi"].as<String>();
// nimi-muuttujaan tallennetaan arvo "Matti"
// as<String>()-funktio muuntaa arvon String-tyyppiseksi
```

JSON-tietojen lukemisen ja kirjoittamisen lisäksi ArduinoJson-liibsi tarjoaa myös mahdollisuuden muuttaa JSON-tiedostoja muihin muotoihin, kuten tavalliseksi tekstimuotoiseksi tai jopa binaariseksi.

## Syvenny

JSON sisältää useita erilaisia tietorakenteita, kuten objekteja, taulukoita ja arvoja. JSON-objektin sisällä olevat tiedot ovat avain-arvo -pareja, jotka ovat yleensä merkkijonona esitettynä.

Esimerkiksi:

```json
{
  "nimi" : "Matti",
  "ikä" : 30
}
```

Avain-arvo -parit erotetaan toisistaan pilkulla ja niiden sisältämät tiedot esitetään kaksoispisteellä. JSON-taulukot puolestaan ovat merkkijonon sisällä sijaitsevia lista-arvoja, jotka ovat eroteltu pilkuilla.

Kun työskentelet ArduinoJson-liibsin kanssa, on tärkeää muistaa varata tarpeeksi muistia JSON-tietojen tallentamiseen. Muistin koko määritellään luotaessa JSON-muuttujaa ja se on riippuvainen siitä, kuinka paljon tietoa halutaan tallentaa.

## Katso myös

Tässä oli lyhyt johdatus JSON-tietojen käsittelyyn Arduino-koodissa. Jos haluat oppia lisää, suosittelemme tutustumaan seuraaviin linkkeihin:

- [ArduinoJson-dokumentaatio](https://arduinojson.org/)
- [JSON-pikakurssi](https://www.json.org/json-fi.html)
- [Arduino ja verkkoviestinnät](https://create.arduino.cc/projecthub/Arduino_Genuino/networking-13d78a)