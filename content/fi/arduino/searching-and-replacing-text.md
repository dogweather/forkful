---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstin etsiminen ja korvaaminen tarkoittaa tietyn merkkijonon löytämistä ja sen muuttamista toiseksi merkkijonoksi koodissasi. Tällä toiminnolla ohjelmoijat voivat tehdä massiivisia muutoksia koodissaan minuuteissa.

## Näin se toimii:
Seuraava esimerkki näyttää, kuinka Arduino-koodissa voidaan etsiä ja korvata tekstiä `replace()`-funktiolla.

```Arduino
String str = "Moi, maailma!";
str.replace("maailma", "Arduino");
Serial.println(str);  // Tulostaa: "Moi, Arduino!"
```
Tässä esimerkissä me etsimme merkkijonoa "maailma" ja korvaamme sen merkkijonolla "Arduino".

## Syvällinen tieto
Etsiminen ja korvaaminen on peräisin varhaisista tekstieditoreista, jotka kehitettiin 1970-luvulla. Arduino tarjoaa monia vaihtoehtoja tämän toiminnallisuuden toteuttamiseksi, mukaan lukien `find()`, `substring()`, ja `replace()`.

`replace()`-funktion käyttö on suoraviivaista: se etsii tiettyä merkkijonoa ja korvaa sen toisella. Jos haluat lisätietoja tämän funktion käyttämisestä, tutustu Arduino String Library -dokumentaatioon.

## Katso myös
- Arduino String Library: www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Arduino Software: www.arduino.cc/en/main/software
- Arduino Forum: forum.arduino.cc

Muistakaa, että olette valmiita tutkimaan lisää ja oppimaan uusia asioita! Jos teillä on kysyttävää, älkää epäröikö osallistua keskusteluun Arduino-foorumilla.