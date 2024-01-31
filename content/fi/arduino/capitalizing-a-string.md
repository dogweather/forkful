---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Isoilla kirjaimilla kirjoittaminen tarkoittaa merkkijonon muuntamista siten, että kaikki kirjaimet ovat suuraakkosia. Ohjelmoijat käyttävät tätä standardoidakseen tekstin esittämisen, helpottamaan vertailua tai korostamaan tietyn tekstin osan tärkeyttä.

## How to: (Kuinka tehdä:)
```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  String message = "Moi Suomi!";
  String upperCaseMessage = message.toUpperCase();
  Serial.println(upperCaseMessage);
  delay(2000); // Odotetaan kaksi sekuntia ennen seuraavaa toistoa.
}
```
Näyteväli: "MOI SUOMI!"

## Deep Dive (Sukellus syvemmälle)
String-objektin muuntaminen kokonaan suuraakkosiin on yksinkertainen toiminto, mutta mutkistuu, jos otetaan huomioon kansainvälinen merkistökoodaus ja eri kielet. Arduino käyttää `toUpperCase()`-metodia, joka on ollut osa kielen standardeja kirjastoja jo vuosia. Tämä metodi käsittelee ANSI:n määrittämiä merkkejä, mutta voi kohdata ongelmia erikoismerkkien tai esimerkiksi skandinaavisten merkkien kanssa. Vaihtoehtoisena ratkaisuna voit käyttää kirjastoja, jotka tukevat laajempaa merkistöä. Kapitalisointiin liittyvä suorituskyky on harvoin huolenaihe, mutta tietyn pituisten merkkijonojen käsittely voi hidastaa mikrokontrollerin toimintaa.

## See Also (Katso myös)
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Unicode-koodaus: https://en.wikipedia.org/wiki/Unicode
- C++ STL string transform -funktio: http://www.cplusplus.com/reference/algorithm/transform/
