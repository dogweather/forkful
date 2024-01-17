---
title:                "Jonojen interpolointi"
html_title:           "Java: Jonojen interpolointi"
simple_title:         "Jonojen interpolointi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi? 
Miksi ohjelmoijat käyttävät interpolointia ja mitä se tarkoittaa?

Interpolointi on yksinkertainen tapa liittää merkkijonoja yhteen säätämällä dynaamisesti toisen merkkijonon sisältöä. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan luoda dynaamisia tulosteita tietokannasta tai käyttäjän syötteestä saatujen tietojen mukaan.

## Miten tehdä se: 
Alla on esimerkki, kuinka interpolointia voidaan käyttää Java-koodissa:

```Java 
String nimi = "Mikko";
String tervehdys = "Hei " + nimi + "!";
System.out.println(tervehdys);
```
Tulostaa: Hei Mikko!

Tässä esimerkissä käytettiin plus-merkkiä liittämään merkkijonot yhteen, mutta Java tarjoaa myös muunlaisia tapoja tehdä interpolointia, kuten String.format() -metodi.

## Sukeltaminen syvemmälle: 
Interpolointi ei ole uusi konsepti ohjelmoinnissa, sillä se juontaa juurensa C-kielestä. Nykyään siihen on kuitenkin tarjolla erilaisia vaihtoehtoja kuten String.format() -metodi ja template engine -kirjastot.

Java:ssa interpolointi tapahtuu hyödyntämällä StringBuilder-oliota, joka sijoittaa merkkijonot dynaamisesti Java Byte -tiedostoon. Tämä lisää suoritusaikaa, mutta tekee interpoloinnista turvallisempaa ja sallii monimutkaisempia operaatioita kuin suora String-merkkijonon liittäminen.

## Katso myös: 
- Java:n virallinen dokumentaatio: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)
- Java String.format() -esimerkkejä: https://dzone.com/articles/java-string-format-examples