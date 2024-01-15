---
title:                "Merkkijonon pituuden löytäminen."
html_title:           "Arduino: Merkkijonon pituuden löytäminen."
simple_title:         "Merkkijonon pituuden löytäminen."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Hakemalla merkkijonon pituuden voidaan helposti tarkistaa merkkijonon sisältämien merkkien määrä. Tämä voi olla hyödyllistä esimerkiksi merkkijonojen käsittelyssä tai vertailussa.

## Miten
```Arduino
String s = "Hei maailma!"; // Alustetaan merkkijono
int length = s.length(); // Käytetään length()-funktiota saadaksemme merkkijonon pituuden
Serial.println(length); // Tulostetaan pituus sarjaporttiin
```

Tuloste: 12

## Syventyminen
Merkkijonojen pituuden hakeminen tapahtuu käyttämällä String-oliota ja sen sisäänrakennettua length()-funktiota. Funktio palauttaa kokonaisluvun, joka kertoo merkkijonon sisältämien merkkien määrän.

Huomioitavaa on, että merkkijonon pituus ei sisällä lopetusmerkkiä (esim. \0), jos sitä käytetään osana merkkijonoa. Lisäksi merkkijonon pituus voi vaihdella eri ohjelmointikielillä riippuen siitä, mitä merkkijonon muodostavat merkit ovat.

## Katso myös
- Reference manual for Arduino String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- String length() function: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/