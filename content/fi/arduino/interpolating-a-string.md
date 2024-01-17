---
title:                "Merkkijonon interpolointi"
html_title:           "Arduino: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä se on ja miksi?
Interpolointi tarkoittaa merkkijonon sisällyttämistä toiseen merkkijonoon. Tämä on hyödyllistä mm. silloin, kun halutaan luoda dynaamisia lauseita, joissa on usein vaihtuvia tietoja.

## Näin teet sen:
```
Arduino ...

String nimi = "Siiri";
int ikä = 25;

String tervehdys = "Hei " + nimi + ", olet " + String(ikä) + " vuotta vanha.";

Serial.println(tervehdys);

```
Tulostus:
```
Hei Siiri, olet 25 vuotta vanha.
```

## Syväsukellus:
Interpolointi on käytössä useimmissa ohjelmointikielissä. Toisissa kielissä se tapahtuu automaattisesti ilman String-luokan avulla tapahtuvaa merkkijonon muotoilua. Arduino-koodissa se kuitenkin vaatii String-luokan käyttöä ja vastaavasti toisissa mikroprosessoriympäristöissä voi olla muita tapoja interpoloida merkkijonoja.

## Katso myös:
[Arduino String-luokka](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
[Intro to Strings in C](https://www.geeksforgeeks.org/strings-c-2/)
[Miksi merkkijonojen manipulointi on tärkeää?](https://techdifferences.com/difference-between-c-and-java.html)