---
title:                "Alirivien erottaminen"
html_title:           "Arduino: Alirivien erottaminen"
simple_title:         "Alirivien erottaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit erotella alimerkkijonoja? Alimerkkijonot ovat käteviä tapoja käsitellä tekstiä, kun haluat erottaa tietyt osat tekstistä tai muuttaa niitä. Esimerkiksi voit käyttää alimerkkijonoja datan käsittelyyn, kuten lämpötila- tai kosteusarvojen hävittämiseen sensoreilta.

## Kuinka tehdä se

```Arduino
String teksti = "Hei maailma!";
String alimerkkijono = teksti.substring(0, 3);

Serial.println(alimerkkijono);

//Tulostaa "Hei"
```

Voit käyttää `substring()` funktiota erotellaksesi tekstiä haluamillesi osille. Ensimmäisenä parametrina annetaan alkuindeksi ja toisena parametrina loppuindeksi. Alkuindeksi on mukana alimerkkijonossa, mutta loppuindeksi ei. Voit myös käyttää `substring()` funktiota ilman toista parametria, jolloin alimerkkijono ulottuu loppuun saakka. 

```Arduino
String teksti = "Hei maailma!";
String alimerkkijono = teksti.substring(4);

Serial.println(alimerkkijono);

//Tulostaa "maailma!"
```

Voit myös tallentaa alimerkkijonon omaan String-muuttujaan kätevyyden vuoksi.

## Syvempi sukellus

Alimerkkijonot eivät toimi pelkästään String-muuttujilla, vaan niitä voi käyttää myös char-taulukoilla ja C-tyylisillä merkkijonoilla. Syvällisempi ymmärrys alimerkkijonoista voi auttaa muissakin ohjelmointitehtävissä, kuten tiedostojen käsittelyssä.

Muista, että alimerkkijonojen indeksointi alkaa aina nollasta ja loppuindeksi ei ole osa alimerkkijonoa.

## Katso myös

- [String-luokan dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Substring opas](https://www.arduino.cc/reference/en/language/variables/variable-scope/substring/)