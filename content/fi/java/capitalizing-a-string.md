---
title:                "Merkitsevä ketju isoilla kirjaimilla"
html_title:           "Java: Merkitsevä ketju isoilla kirjaimilla"
simple_title:         "Merkitsevä ketju isoilla kirjaimilla"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Stringin isojen kirjainten käyttö tarkoittaa merkkijonon ensimmäisen kirjaimen muuttamista isoksi kirjaimeksi ja loppuosan kirjainten muuttamista pieniksi kirjaimiksi. Tämä tehdään usein ohjelmoinnissa, jotta merkkijonojen ulkoasu olisi yhtenäinen ja paremmin luettavissa.

## Kuinka tehdä:
Esimerkkikoodi Java-koodilohkoissa näyttää, kuinka muuntaa merkkijono isojen kirjainten käyttöön ja mikä on lopputulos.
```Java
// Stringin isojen kirjainten käyttö
String s = "stringin isojen kirjainten käyttö";
System.out.println("Alkuperäinen merkkijono: " + s);
System.out.println("Merkkijono isojen kirjainten käytöllä: " + s.toUpperCase());

//Mikä on lopputulos:
Alkuperäinen merkkijono: stringin isojen kirjainten käyttö
Merkkijono isojen kirjainten käyttö: STRINGIN ISOJEN KIRJAINTEN KÄYTTÖ
```

## Syvemmälle:
Stringien isojen kirjainten käyttö on ollut osa ohjelmointia jo vuosikymmenten ajan. Tietyissä ohjelmointikielissä, kuten C:llä ja BASIC:lla, kirjaimien käsitteleminen tapahtuu manuaalisesti merkkien ASCII-koodien avulla. Jotkin modernit ohjelmointikielet, kuten Python, tarjoavat valmiin funktion merkkijonon isojen kirjainten käyttöön. Java-kielessä tätä tehtävää varten löytyy metodi ```toUpperCase()```.

## Katso myös:
Lisää tietoa merkkijonojen muokkaamisesta ja Java-kielestä saat Java-oppaista ja verkkosivuilta, kuten Stack Overflow ja Oracle.