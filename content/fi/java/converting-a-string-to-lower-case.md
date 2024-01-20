---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon muuttaminen pieniksi kirjaimiksi on tehtävä, jossa muunnetaan kaikki merkkijonon kirjaimet pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän usein tekstin normalisoimiseksi ja vertailun helpottamiseksi, koska järjestelmät erottelevat usein isot ja pienet kirjaimet.

## Miten se tehdään:

Alla on koodiesimerkkejä Java-ohjelmointikielellä:

```Java
String isoKirjain = "Moi Moikka!";
String pieniKirjain = isoKirjain.toLowerCase();
System.out.println(pieniKirjain);
```

Tämän koodin tuloste on:

```
moi moikka!
```

## Syväsukellus:

Historiallisesti merkkijonojen pienten kirjainten käyttö on juurtunut ajatukseen, että tekstien vertaaminen olisi helpompaa ilman isoja kirjaimia. Tämä on järkevää, koska esimerkiksi käyttäjätunnukset ja salasanat ovat yleensä case-sensitive, eli otetaan huomioon isojen ja pienten kirjainten ero.

Vaihtoehtoisesti, `toUpperCase()` -metodi muuttaa kaikki merkkijonon kirjaimet isoiksi kirjaimiksi. Tämä voi olla hyödyllistä joissakin tilanteissa, riippuen sovelluksen käytännöistä ja vaatimuksista.

Java'n `toLowerCase()` ja `toUpperCase()` metodit käyttävät UnicodeData-tiedostoa määrittämään, onko merkki kirjain ja jos on, mikä on sen vastine isolla tai pienellä kirjaimella.

## Katso myös:

Jos haluat tutkia lisää merkkijonojen käsittelyä ja manipulointia Javassa, tarkista seuraavat linkit:

1. Oracle Java Docs for String: [Linkki](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
2. Java String toLowerCase() -metodi: [Linkki](https://www.javatpoint.com/java-string-tolowercase)
3. Java String toUpperCase() -metodi: [Linkki](https://www.javatpoint.com/java-string-touppercase)