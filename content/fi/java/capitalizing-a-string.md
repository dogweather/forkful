---
title:                "Java: Merkkijonon muuttaminen isolla alkukirjaimella"
simple_title:         "Merkkijonon muuttaminen isolla alkukirjaimella"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi: 
Kapitalisointi on tapa muuttaa merkkijonon kirjaimien koon alkuperäisestä muodosta isoksi tai pieneksi. Se voi olla hyödyllistä esimerkiksi tekstin muotoilussa tai tiedon käsittelyssä.

## Miten: 
Kapitalisoiminen on helppoa Java-ohjelmointikielessä. Voit käyttää valmiita kirjastoja tai kirjoittaa oman funktion. Esimerkiksi String-luokassa on valmiina metodeja capitalize() ja toUpperCase(), joilla voi muuttaa merkkijonon kirjaimet isoksi. Alla on esimerkki tällaisen funktion käytöstä:

```Java
String teksti = "tämä on esimerkkiteksti";
String kapitaloitu = teksti.toUpperCase();
System.out.println(kapitaloitu);
```

Tämä koodi tulostaa merkkijonon "TÄMÄ ON ESIMERKKITEKSTI", jossa kaikki kirjaimet ovat isolla.

## Syvällisempi sukellus: 
Yksi asia, joka kannattaa huomioida kapitalisoinnissa, on merkistö. Eri kielissä ja kulttuureissa käytetään erilaisia merkkejä, joten kapitalisointi voi antaa erilaisia tuloksia eri ympäristöissä. Esimerkiksi ääkköset eivät välttämättä muutu suuriksi kirjaimiksi kaikissa järjestelmissä. Myös numerot ja erikoismerkit voivat aiheuttaa ongelmia kapitalisoitaessa tekstejä.

Java tarjoaa vaihtoehtoisia metodeja, kuten toUpperCase(Locale), jonka avulla voi määrittää tarkemmin halutun merkistön. Lisäksi on olemassa tapoja käsitellä erikoismerkkejä ja muuttaa ne halutuiksi kirjaimiksi.

## Katso myös: 
- [String-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Strings -kapitalisoinnin ohjeet](https://www.javatpoint.com/java-string-to-uppercase)
- [Eri merkistöjen vaikutus Java Strings -kapitalisointiin](https://docs.oracle.com/javase/tutorial/i18n/text/string.html) (englanniksi)