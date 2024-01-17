---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Arduino: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

"Havainnollisesti selitettynä, merkkijonon pituuden löytäminen tarkoittaa merkkijonon sisältämien merkkien määrän laskemista. Tätä tarvitaan usein esimerkiksi ohjelmoinnissa, jotta voidaan varmistaa, että merkkijono mahtuu koodiin määriteltyyn muuttujan kokoon ilman että tietoa katoaa."

## Miten:

Kokeile seuraavaa koodia:
```Arduino
char string[] = "Tämä on merkkijono!";
int len = strlen(string);
Serial.println(len);
```
Tulosteessa pitäisi näkyä luku 21, mikä vastaa merkkijonon "Tämä on merkkijono!" pituutta.

## Purenta Pohjalle

Tutuksi tulleen termin "string length" takana piilee C-kielen funktio "strlen()", jota käytetään myös monissa muissa ohjelmointikielissä. Tämä funktio etsii ja laskee merkkijonon lopun ilmaistuna null-muuttujalla, ja palauttaa sitten merkkijonon pituuden nollahetkellä.

"String length" on yksi tärkeä osa merkkijonon käsittelyä, mutta on olemassa myös muita funktioita ja tapoja, joilla voidaan manipuloida ja löytää merkkijonojen pituus. Jokaisella kielellä on omat erityispiirteensä ja hyödylliset työkalunsa, joten kannattaa tutustua erilaisiin vaihtoehtoihin ja löytää itselleen sopiva tapa käsitellä merkkijonoja.

## Katso myös:

[Lisätietoa Arduino string_length -toiminnosta](https://www.arduino.cc/reference/en/language/functions/string/stringlength/) 
Jos olet kiinnostunut opettelemaan lisää merkkijonojen käsittelystä C-kielessä, suosittelemme tutustumaan [tähän opetusmateriaaliin](https://www.programiz.com/c-programming#string-handling).