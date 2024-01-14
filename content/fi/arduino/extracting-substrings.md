---
title:                "Arduino: Alimerkkijonojen erottaminen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottamisella on monia hyödyllisiä sovelluksia Arduino-ohjelmoinnissa. Esimerkiksi voit käyttää substringeja käsittelemään tekstin syötteitä, kirjoittamaan parempaa käyttöliittymää ja muokkaamaan datan tallennusta. Jatka lukemista ja opi, kuinka voit hyötyä substringien käytöstä omassa projektissasi.

## Kuinka

Substringien erottaminen Arduino-ohjelmoinnissa on helppoa käyttämällä sisäistä `substring()` funktiota. Tämä funktio ottaa parametreina merkkijonon alkupaikan, loppupaikan ja tarvittaessa päämerkin. Tässä esimerkissä otamme toisen ja neljännen merkin 'Arduino' merkkijonosta ja tulostamme sen sarjaanliitetyssä muodossa.

```Arduino
String nimi = "Arduino";
String alikirjain = nimi.substring(1,4);
Serial.println(alikirjain);
```

Tulostus: `rdu`

On tärkeää huomata, että merkkijonon indeksointi alkaa aina nollasta. Joten ensimmäinen merkki on indeksillä 0 ja viimeinen merkki on pituuden vähennettynä yhdellä.

Päämerkin asettaminen on valinnainen ja oletusarvoisesti se on tyhjä merkki. Tässä esimerkissä otamme kolmannen merkin 'Arduino' merkkijonosta ja käytämme pilkkua päämerkkinä.

```Arduino
String nimi = "Arduino";
String alikirjain = nimi.substring(2, ',');
Serial.println(alikirjain);
```

Tulostus: `d`

## Syvällinen sukellus

Substringien avulla voit myös luoda uusia merkkijonoja ja tallentaa ne muuttujina. Tässä esimerkissä luomme uuden merkkijonon `nimi2` käyttämällä `substring()` funktiota yhdistämällä `nimi` ja `alikirjain` merkkijonot.

```Arduino
String nimi = "Arduino";
String alikirjain = nimi.substring(1,4);
String nimi2 = nimi + alikirjain;
Serial.println(nimi2);
```

Tulostus: `Arduirdu`

Myös `substring()` funktiolla voi käsitellä erilaisia syötteitä, kuten käyttäjän syöttämiä tietoja tai sarjaväylän lukemia arvoja. Voit myös yhdistää useita `substring()` funktioita luomaan monimutkaisempia merkkijonoja.

## Katso myös

- [String luokan dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/substring/)
- [Esimerkkikoodi String luokalle](https://www.arduino.cc/en/Tutorial/StringConstructors)
- [Selitys merkkijonon indeksoinnista](https://www.arduino.cc/en/Reference/StringIndexof)