---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Java: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#Mitä & Miksi?
Merkkijonon pituuden löytäminen on yksinkertainen tehtävä, joka löytyy useimmista ohjelmistokehityksessä käytetyistä ohjelmointikielistä. Tämä tarkoittaa, että ohjelmoijat voivat helposti selvittää, kuinka monta merkkiä tekstissä on. Tätä tietoa tarvitaan usein, esimerkiksi tarkistettaessa, onko annettu syöte liian pitkä tai lyhyt.

#Näin teet sen:
```Java
String merkkijono = "Tämä on esimerkki!";
int pituus = merkkijono.length();
System.out.println("Merkkijonon " + merkkijono + " pituus on " + pituus);
```
**Tuloste**: Merkkijonon Tämä on esimerkki! pituus on 19.

Toinen tapa löytää merkki-jonon pituus on käyttää ```str.toCharArray()``` -metodia ja laskea taulukon sisältämien merkkien määrä.

```Java
String merkkijono = "Tämä on esimerkki!";
char[] merkit = merkkijono.toCharArray();
int pituus = merkit.length;
System.out.println("Merkkijonon " + merkkijono + " pituus on " + pituus);
```
**Tuloste**: Merkkijonon Tämä on esimerkki! pituus on 19.

#Syvällisemmin:
Merkkijonon pituuden laskemisella on pitkä historia ohjelmoinnissa johtuen sen hyödyllisyydestä eri tilanteissa. Sen lisäksi, että se auttaa tarkistamaan syötteen oikeellisuuden, sitä käytetään myös tekstin käsittelyssä ja järjestämisessä.

Muita vaihtoehtoja merkki-jonon pituuden laskemiseen ovat esimerkiksi ```str.codePointCount(int, int)``` ja ```str.length(int)```, jotka mahdollistavat tarkemman kontrollin merkkien lukumäärästä.

Java käyttää String-luokassa pituuden määrittämiseen kaksi muuttujaa: ```count``` ja ```value```. Count pitää kirjaa merkkijonon pituudesta, ja siinä ei oteta huomioon mahdollisia erikoismerkkejä, kuten ääkkösiä. Value puolestaan sisältää itse merkkijonon, josta pituus voidaan laskea.

#Katso myös:
- [Java-merkkijonon dokumentaatio](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#length())
- [Tietoja syötteiden käsittelystä ja validoinnista Java-sovelluksissa](https://www.baeldung.com/java-validate-input)