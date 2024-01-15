---
title:                "Merkkijonon muotoilu"
html_title:           "Arduino: Merkkijonon muotoilu"
simple_title:         "Merkkijonon muotoilu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointitilanteissa tarvitaan muotoiltuja merkkijonoja. Yksi yleinen muotoilu on tehdä merkkijonon ensimmäisestä kirjaimesta iso alkukirjain ja lopuista kirjaimista pieniä. Tähän tarvitaan pienen kikkailun lisäksi myös koodia, ja tässä artikkelissa käymme läpi miten se tehdään Arduino-ympäristössä.

## Käyttöohje

Aloita luomalla uusi Arduino-tiedosto ja määrittele siinä ensin käsiteltävä merkkijono, esimerkiksi "hello world". 
```
Arduino

String s = "hello world";
```
Seuraavaksi käytetään String-olioon kuuluvaa `capitalize`-funktiota, joka tekee halutun muotoilun merkkijonolle. Lopuksi tulostetaan muotoiltu merkkijono sarjamonitorille. 
```
Arduino

s.capitalize();
Serial.println(s);
```
Tulosteena pitäisi nyt olla "Hello world". 

## Syvässä Vesi

String-luokka sisältää muitakin hyödyllisiä funktioita, joilla voidaan helposti muotoilla merkkijonoja eri tavoin. `toLowerCase()` muuttaa kaikki merkit pieniksi ja `toUpperCase()` kaikki merkit isoiksi. Lisäksi `substring()`-funktio leikkaa merkkijonosta tietyn osan ja `indexOf()`-funktio etsii halutun merkkijonon ensimmäisen esiintymän. Näitä funktioita ja muita String-luokan toimintoja kannattaa tutkia tarkemmin, sillä ne voivat tehdä merkkijonojen muokkaamisesta paljon helpompaa ja tehokkaampaa.

## Katso Myös

- [String-luokan dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Koodiesimerkkejä String-luokan käytöstä](https://create.arduino.cc/projecthub/tags/strings)