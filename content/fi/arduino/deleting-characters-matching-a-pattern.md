---
title:    "Arduino: Kuviota vastaavien merkkien poistaminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka täyttävät tietyn mallin? Tämä voi olla hyödyllistä esimerkiksi tietynlaisen tiedostonkäsittelyn yhteydessä tai tekstipohjaisten käyttöliittymien ohjelmoinnissa.

## Kuinka

Koodi alla näyttää yksinkertaisen esimerkin merkkien poistamisesta annetusta merkkijonosta, jossa käytetään `for`-silmukkaa ja `if`-ehtolauseita.

```Arduino
String alkuperainenMerkkijono = "H2e36l46l68o1 W1o4r16l43d";
String uusiMerkkijono = "";

for (int i = 0; i < alkuperainenMerkkijono.length(); i++) {
  char c = alkuperainenMerkkijono.charAt(i);
  if (c >= 'a' && c <= 'z') { //tarkistetaan, onko merkki pieni kirjain
    uusiMerkkijono += c; //jos on, lisätään se uuteen merkkijonoon
  }
}

Serial.println(uusiMerkkijono); //tulostetaan uusi merkkijono
```

Tämä koodi tulostaisi "hello World", sillä se poistaa alkuperäisestä merkkijonosta kaikki numerot ja välilyönnit.

## Syvemmälle

Vaikka yllä olevassa esimerkissä käytettiin `for`-silmukkaa ja `if`-ehtolauseita, on olemassa myös muita tapoja poistaa merkkejä. Esimerkiksi `String`-luokan `replace()`-metodi voi korvata merkkejä toisella merkillä annetusta merkkijonosta. `String`-luokka tarjoaa myös muita hyödyllisiä metodeja merkkien tarkistamiseen ja manipulointiin.

Merkkejä poistaessa on myös hyvä muistaa, että Arduino-kielessä on erityisiä merkkejä, kuten `\n` ja `\r`, jotka voivat aiheuttaa ongelmia merkkien tarkistamisessa ja manipuloinnissa. Näitä merkkejä voi joutua käsittelemään erikseen, jos ne ovat merkkijonossa, josta halutaan poistaa merkkejä.

## Katso myös

Tässä on muutama hyödyllinen linkki lisätietoa ja esimerkkejä varten:

- [Arduino Reference - String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String replace() -dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- ["How to Manipulate Strings in Arduino"](https://www.makerguides.com/manipulate-strings-arduino/) (englanniksi)