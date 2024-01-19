---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon pituuden selvittäminen tarkoittaa merkkien lukumäärän laskemista merkkijonossa. Ohjelmoijat tekevät tämän yleensä määrittääkseen, paljonko muistia merkkijono vie tai navigoidakseen merkkijonossa.

## Kuinka:
Arduino-ohjelmointiympäristössä merkkijonon pituus voidaan selvittää funktion `strlen()` avulla. Tässä esimerkki koodista:

```Arduino
void setup() {
  Serial.begin(9600);
  char merkkijono[] = "Moi, Suomi!";
  Serial.println(strlen(merkkijono));
}
void loop() {
  // toiminnot tähän
}
```

Saatu tuloste:

```Arduino
11
```

## Syvempi Sukellus:
Historiallisessa yhteydessä, merkkijonon pituuden määrittely on ollut osa ohjelmointia sen alkuajoista lähtien. Se on oleellinen toiminto tiedon käsittelyssä ja hallinnassa.

Vaihtoehtoina, eri ohjelmointikielissä tarjotaan usein funktioita merkkijonon pituuden määrittämiseen, esimerkiksi `length()` Javassa ja `len()` Pythonissa.

Arduinossa, `strlen()` löytyy standardista C kirjastosta ja se laskee merkkijonon pituuden etsimällä merkkijonon loppua ilmaisevan nollabyten `\0`.

## Katso myös:
1. Arduino “strlen” - https://www.arduino.cc/reference/en/language/functions/character-functions/strlen/
2. Merkkijonojen käsittely C-ohjelmoinnissa - http://users.jyu.fi/~pommi/Ctyohyv2.htm
3. Merkkijonofunktiot C-kielessä (strlen) - https://www.learn-hsk.com/c-merkkijonot.html