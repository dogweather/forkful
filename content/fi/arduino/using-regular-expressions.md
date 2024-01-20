---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Säännölliset lausekkeet (regular expressions, regex) ovat työkalu tekstin hakuun, korvaukseen ja analysointiin. Ohjelmoijat käyttävät niitä koska ne tarjoavat tarkkuutta ja monipuolisuutta tekstin käsittelyssä.

## Näin se toimii:

Tässä on esimerkki säännöllisen lausekkeen käytöstä Arduino-koodissa:

```Arduino
#include <regex.h>
match_t match;
regex_t myRegex;

void setup() {
re_comp(&myRegex, "^[a-z]{2,5}$");
Serial.begin(9600);
}

void loop() {
  if (re_matchp(&myRegex, "Hello", &match) > 0) {
    Serial.println("Lauseke vastaa");
  } else {
    Serial.println("Lauseke ei vastaa");
  }
}
```

Tässä esimerkissä regex käytetään tarkistamaan, vastaako syöttöteksti (tässä tapauksessa "Hello") määriteltyä säännöllistä lauseketta `^[a-z]{2,5}$`. Jos vastaa, tulostetaan "Lauseke vastaa", muuten "Lauseke ei vastaa".

## Syvempi sukellus:

Säännölliset lausekkeet ovat peräisin 1950-luvulta ja ne ovat kehittyneet paljon vuosikymmenten aikana. Arduino-ohjelmointiympäristössä regex-kirjaston käyttö voi olla haastavaa, koska se on resurssitehokas ja siihen liittyy monimutkaisia ohjelmointirakenteita.

Regexille vaihtoehtoja ovat esimerkiksi tavalliset merkkijonotoiminnot, kuten String.find() tai String.substring() Arduino-kirjastossa. Nämä toiminnot ovat yksinkertaisempia käyttää, mutta ne eivät ole yhtä joustavia eivätkä yhtä tehokkaita kuin regexit.

## Katso myös:

Jos haluat tietää lisää säännöllisten lausekkeiden käytöstä Arduinossa, voit tutustua seuraaviin lähteisiin:

- Arduino Regex kirjasto: [Arduino-Regex](https://github.com/nickgammon/Regexp)

- Säännölliset lausekkeet: [Regular-Expressions.info](http://www.regular-expressions.info/)

- Arduino String kirjasto: [Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)