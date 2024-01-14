---
title:    "Arduino: Merkkijonon pituuden löytäminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joissakin Arduino-projekteissa saattaa olla tarvetta selvittää merkkijonon pituus. Esimerkiksi tekstiä tulostava laite voi tarvita tietoa siitä, kuinka monta merkkiä merkkijonossa on, jotta teksti tulostuu halutun mittaiseksi. Tässä blogikirjoituksessa käymme läpi kuinka voit helposti selvittää merkkijonon pituuden Arduino-ohjelmoinnissa.

## Kuinka tehdä

Merkkijonon pituuden selvittäminen Arduino-ohjelmoinnissa onnistuu käyttämällä `strlen()`-funktiota. Tämä funktio löytyy Arduino String-kirjastosta ja se laskee annetun merkkijonon sisältämien merkkien määrän. Alla olevassa esimerkissä luomme ja tulostamme merkkijonon pituuden.

```Arduino
#include <string.h>

void setup() {
  Serial.begin(9600); // alustetaan sarjaportti
  String s = "Tämä on teksti"; // luodaan merkkijono
  int len = s.length(); // käytetään strlen()-funktiota selvittää merkkijonon pituus
  Serial.println(len); // tulostetaan pituus sarjaporttiin
}

void loop() {

}
```

Yllä olevan koodin tuloste on `15`, koska merkkijonossa "Tämä on teksti" on 15 merkkiä. Voimme myös tulostaa merkkijonon pituuden suoraan käyttämällä `s.length()`-funktiota sen sijaan, että tallentaisimme sen muuttujaan.

Arduino String-kirjasto tarjoaa myös muita hyödyllisiä funktioita merkkijonon käsittelyyn, kuten `charAt()`, joka antaa pääsyn tiettyyn merkkiin merkkijonossa.

## Syvemmälle

Merkkijonot ovat erittäin tärkeitä ohjelmoinnissa ja usein joudumme käsittelemään niitä eri tavoin. Arduino String-kirjasto tarjoaa monipuoliset työkalut merkkijonon manipulointiin ja käsittelyyn. Joitakin muita hyödyllisiä funktioita ovat esimerkiksi `substring()`, jolla voidaan ottaa osa merkkijonosta, ja `replace()`, jolla voidaan korvata merkkijonon osia. Kannattaa tutustua tarkemmin näihin funktioihin ja löytää niiden avulla uusia tapoja käsitellä merkkijonoja.

## Katso myös

- [String-tiedon tyypit Arduino-ohjelmoinnissa](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Esimerkkejä String-kirjaston käytöstä](https://www.arduino.cc/en/Tutorial/StringConstructorExample)
- [Käyttöohjeet Arduino String-kirjastolle](https://github.com/arduino-libraries/String/blob/master/README.md)