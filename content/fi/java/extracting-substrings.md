---
title:                "Alialirivien erottaminen"
html_title:           "Java: Alialirivien erottaminen"
simple_title:         "Alialirivien erottaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Substringien erottaminen tarkoittaa osan merkkijonon poimimista. Tämä voi olla kätevää tietyissä tilanteissa, kuten kun halutaan käsitellä tiettyjä osia tekstistä erikseen. Ohjelmoijat käyttävät sitä usein helpottaakseen tiettyjen toimintojen toteuttamista.

## Miten?
Alla on esimerkkejä siitä, miten substringeja voidaan erottaa käyttämällä Java-ohjelmointikieltä.

```
String s = "Tämä on esimerkki tekstistä";
String substring = s.substring(4, 9);
System.out.println(substring);
```
Tämä tulostaisi "on es", koska se erottaa tekstin osat väliltä 4-9 ja tallentaa ne muuttujaan "substring". Voit myös erottaa tekstistä osan aloittamalla tietystä merkistä ja lopettamalla siihen asti, kun teksti päättyy.

```
String s = "Tämä on toinen esimerkki";
String substring = s.substring(9);
System.out.println(substring); 
```
Tämä tulostaisi "toinen esimerkki", koska se erottaa tekstin osan väliltä 9-14 ja tallentaa sen muuttujaan "substring". Voit myös käyttää substringeja muuttujien arvojen muokkaamiseen tai tarkistamiseen.

## Syvempi sukellus
Substringien erottaminen oli ensimmäisen kerran mahdollista Java 1.2 -versiosta, kun lisättiin toinen substring-metodi, joka otettiin käyttöön Java 7:ssä. On myös erilaisia tapoja erottaa substringeja, kuten käyttämällä split-metodia tai regex-formaatteja. Substringien erottamisen avulla voit käsitellä tekstiä tehokkaammin ja tarkemmin.

## Katso myös
Lisätietoa substringien erottamisesta Java-ohjelmointikielessä löytyy virallisilta Java-sivuilta: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int)

Voit myös lukea lisää substringien käytöstä tästä artikkelista: https://www.baeldung.com/java-string-substr