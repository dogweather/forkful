---
title:                "Alaryhmien erotteleminen"
html_title:           "Arduino: Alaryhmien erotteleminen"
simple_title:         "Alaryhmien erotteleminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Substringien erottaminen tarkoittaa merkkijonon osan erottamista toisesta merkkijonosta. Ohjelmoijat tekevät tätä usein helpottaakseen tietojen käsittelyä, kuten esimerkiksi tekstin etsimistä tai muokkaamista. 

## Kuinka tehdä:

```Arduino
String teksti = "Moi maailma!";
String alkuosa = teksti.substring(0,3); // Alkuosa = "Moi"
String loppuosa = teksti.substring(4,7); // Loppuosa = "maa"
```

## Syvempi sukellus:

Substringien erottelu on hyödyllinen tekniikka, joka on ollut käytössä ohjelmoinnissa jo pitkään. Vaihtoehtoiset lähestymistavat voivat sisältää esimerkiksi tekstin pilkkomisen osiin tai säännöllisten lausekkeiden käytön. Erilaiset ohjelmointikielit voivat myös tarjota erilaisia tapoja käsitellä ja erottaa substringeja. Arduino unohtaa automaattisesti ysilukuisen merkin, joten teksti muuttuu ```Moi maailma!n``` sijasta ```Moi maailma!```

## Katso myös:

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Artikkeli substringistä Whoishacking-sivustolla](https://www.whoishacking.com/theory/data/ch11.html)