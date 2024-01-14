---
title:    "Arduino: Säännöllisten ilmaisujen käyttö"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

#
## Miksi

Arduino on yksi suosituimmista avoimen lähdekoodin elektroniikkaprotokollista ja kehitysalustoista. Sen avulla voit helposti ja edullisesti luoda monenlaisia elektronisia projekteja. Yksi Arduino-kehitysalustan hienouksista on sen kyky käyttää säännöllisiä lausekkeita, joiden avulla voit tunnistaa ja käsitellä erilaisia merkkijonoja. Tässä blogikirjoituksessa opit kuinka hyödyntää näitä säännöllisiä lausekkeita Arduino-projekteissasi!

## Kuinka tehdä se

Käyttäessäsi säännöllisiä lausekkeita Arduino-koodissasi, sinun täytyy ensin sisällyttää "regex" kirjasto koodiisi:

```Arduino
#include <regex.h>
```

Sen jälkeen voit käyttää säännöllisiä lausekkeita esimerkiksi "match" ja "replace" funktioiden avulla. Tässä on yksinkertainen esimerkki, jossa muutetaan "Hello World" merkkijonoksi "Hi World":

```Arduino
String myString = "Hello World";
regexReplace(myString, "Hello", "Hi");

Serial.println(myString); // Tulostaa "Hi World"
```

Voit myös käyttää säännöllisiä lausekkeita tarkistamaan, onko annettu merkkijono oikeassa muodossa:

```Arduino
String myString = "123456";

if(regexMatch(myString, "[0-9]+")){
  // Tee jotain jos merkkijono koostuu vain numeroista
}
```

Huomaa, että säännöllisiä lausekkeita käytettäessä, merkkijonojen täytyy olla String-tyyppisiä. Sinun täytyy myös käyttää "regexMatch" tai "regexReplace" funktioiden sijaan "match" tai "replace" funktioita.

## Syvällinen sukellus

Säännölliset lausekkeet ovat erittäin kätevä työkalu tekstin käsittelyssä ja pääset vielä syvemmälle, kun opit käyttämään lausekkeita eri tavoin. Tässä muutamia esimerkkejä:

- Voit käyttää säännöllisiä lausekkeita tunnistamaan ja korvaamaan tiettyjä merkkejä tai sanoja merkkijonossa.
- Voit käyttää erilaisia säännöllisiä lausekkeita tarkistamaan, onko merkkijono oikeassa muodossa, esimerkiksi tarkistamaan, onko sähköpostiosoite oikeassa muodossa.
- Voit käyttää säännöllisiä lausekkeita luomaan monimutkaisempia ehtolausekkeita, jotka perustuvat merkkijonon sisältöön.

Kannattaa tutustua tarkemmin säännöllisiin lausekkeisiin ja niiden eri käyttötapoihin, jos haluat hyödyntää niitä entistä enemmän Arduino-projekteissasi.

## Katso myös

- [Säännöllisten lausekkeiden opas (englanniksi)](https://www.regular-expressions.info/)
- [Arduino - String-tyypin ohjedokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino - "regex" kirjaston ohjedokumentaatio](https://www.arduino.cc/reference/en/libraries/regex/)

Kiitos, että luit tämän blogikirjoituksen ja toivottavasti se auttaa sinua hyödyntämään säännöllisiä lausekkeita Arduino-projekteissasi!