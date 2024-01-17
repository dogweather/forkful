---
title:                "Työskentely jsonin kanssa"
html_title:           "Arduino: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

JSON eli Javascript Object Notation on yleinen formaatti tietojen tallentamiseen ja vaihtamiseen ohjelmoinnissa. JSONilla voidaan tallentaa ja lähettää tietoa eri ohjelmien ja laitteiden välillä, kuten esimerkiksi web-palvelimien ja Arduino-mikrokontrollerien välillä. Tämä helpottaa tietojen käsittelyä ja antaa mahdollisuuden luoda monimutkaisempia ohjelmia.

## Kuinka:

Esimerkiksi, jos haluat lähettää datan web-palvelimelle JSON-muodossa ja vastaanottaa sitä Arduino-mikrokontrollerillesi, voit käyttää seuraavaa koodia:

```Arduino
// Kirjasto JSON-käsittelyyn
#include <ArduinoJson.h>

// Alustetaan JSON-objekti
StaticJsonDocument<200> doc;

// Lisätään data objektiin
doc["nimi"] = "Matti";
doc["ikä"] = 35;

// Muutetaan objekti JSON-muotoon
String json = "";
serializeJson(doc, json);

// Lähetetään data web-palvelimelle (esimerkiksi POST-pyynnöllä)
// ja vastaanotetaan vastaus
// ...koodi jätetty pois selkeyden vuoksi...

// Parsitaan vastaanotettu JSON-data 
const size_t capacity = JSON_OBJECT_SIZE(2);
StaticJsonDocument<capacity> doc2;
deserializeJson(doc2, json);

// Haetaan data JSON-objektista
String nimi = doc2["nimi"];
int ikä = doc2["ikä"];

// Tulostetaan data sarjaporttiin
Serial.println(nimi);
Serial.println(ikä);

```

Esimerkissä käytetään ArduinoJson-kirjastoa, joka helpottaa JSON-muodon käsittelyä ja parsimista. Kirjasto täytyy asentaa ennen sen käyttöä. Esimerkissä käytetään myös POST-pyyntöä, joka tarvitsee omat lisäkoodinsa lähettämiseen ja vastaanottamiseen. 

## Deep Dive:

JSON on kevyt ja yksinkertainen formaatti, joka syntyi tarpeesta korvata hankalammin käytettävä XML-tiedostoformaatti. JSON-muotoa voi verrata esimerkiksi YAML:iin ja CSV:hen, mutta JSON on yleisempi ja helpommin luettavissa ohjelmoijan näkökulmasta.

JSONia voi käyttää myös vaihtoehtona relaatiotietokannoille ja XML-tiedostoille. Se on nopeampi ja vaatii vähemmän tilaa kuin XML, mutta ei kuitenkaan ole yhtä rakenteellinen tietokannan kanssa kuin relaatiotietokannat. 

## Katso myös:

- [ArduinoJSON-kirjaston dokumentaatio](https://arduinojson.org/)
- [JSON-wikipedia-artikkeli](https://fi.wikipedia.org/wiki/JavaScript_Object_Notation)
- [JSON-opetusvideo](https://www.youtube.com/watch?v=iiADhChRriM)