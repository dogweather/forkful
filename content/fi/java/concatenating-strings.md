---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Java: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
"Ohjelmoijat käyttävät usein erilaisia tapoja yhdistää merkkijonoja, eli tekstinpätkiä, yhdeksi kokonaisuudeksi. Tätä kutsutaan merkkijonojen yhdistelemiseksi (concatenating strings) ja se on erittäin tärkeä osa ohjelmoinnin perustaitoja. Tämä mahdollistaa esimerkiksi käyttäjän syöttämien tietojen liittämisen valmiiksi ohjelmoituihin viesteihin tai tulostettaessa tiedot yhdessä halutussa muodossa."

## Miten
"### Esimerkki 1:
```java
String etunimi = "Matti";
String sukunimi = "Meikäläinen";
String kokoNimi = etunimi + " " + sukunimi;
System.out.println(kokoNimi);
```
Tulostaa: Matti Meikäläinen

### Esimerkki 2: 
```java
String toivelause = "Olen " + 27 + " vuotta vanha.";
System.out.println(toivelause);
```
Tulostaa: Olen 27 vuotta vanha.

### Esimerkki 3: 
```java
String tervehdys = "Hei ";
String nimi = "Maria";
String huutomerkki = "!";
System.out.println(tervehdys.concat(nimi).concat(huutomerkki));
```
Tulostaa: Hei Maria!

### Esimerkki 4:
```java
String muuttuja1 = "Tervetuloa";
String muuttuja2 = " Java-maailmaan!";
System.out.println(muuttuja1.concat(muuttuja2));
```
Tulostaa: Tervetuloa Java-maailmaan!"

## Syväsukellus
"Merkkijonojen yhdistelemistä on käytetty ohjelmoinnissa jo pitkään, ja Java-kielen syntaksissa se tehdään yleensä käyttämällä plus-merkkiä (+) tai concat-metodia. Joissain tapauksissa voi olla hyödyllistä käyttää StringBuilder-luokkaa, mikäli tiedetään, että merkkijonojen yhdisteleminen tapahtuu suurella datamäärällä. Tämä johtuu siitä, että StringBuilder-luokka luo uuden merkkijonon tarvittaessa, kun taas String-luokka luo aina uuden merkkijonon joka kerta, kun siihen tehdään muutoksia. Tämä voi aiheuttaa tehokkuusongelmia, jos käytetään paljon merkkijonojen yhdistelemistä."

## Katso myös
- [Java StringBuilder-luokka](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- [Java String-luokka](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Merkkijonojen muokkaaminen Java:ssa](https://www.w3schools.com/java/java_strings.asp)