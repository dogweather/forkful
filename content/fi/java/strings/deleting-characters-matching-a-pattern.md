---
date: 2024-01-20 17:42:33.317475-07:00
description: "How to: - Miten: Poistettaessa merkkej\xE4 Java-ohjelman merkkijonosta\
  \ s\xE4\xE4nn\xF6llisten lausekkeiden avulla k\xE4ytet\xE4\xE4n tyypillisesti `String`-luokan\u2026"
lastmod: '2024-04-05T22:51:10.579579-06:00'
model: gpt-4-1106-preview
summary: "- Miten: Poistettaessa merkkej\xE4 Java-ohjelman merkkijonosta s\xE4\xE4\
  nn\xF6llisten lausekkeiden avulla k\xE4ytet\xE4\xE4n tyypillisesti `String`-luokan\
  \ `replaceAll()`-metodia. T\xE4m\xE4 k\xE4ytt\xE4ytyminen on ollut Java-kieless\xE4\
  \ alkuajoista l\xE4htien, jolloin s\xE4\xE4nn\xF6lliset lausekkeet lis\xE4ttiin\
  \ Java 1.4:\xE4\xE4n. Vaihtoehtoisia tapoja poistaa merkkej\xE4 ovat `replace()`-metodi\
  \ (ilman regex-tukea) ja `Pattern`-luokka (suorituskykyisemp\xE4\xE4 regex-k\xE4\
  sittely\xE4 varten). `Pattern`-luokka esikompiloi regex-kuvion, mik\xE4 on hy\xF6\
  dyllist\xE4, jos samaa kuviota k\xE4ytet\xE4\xE4n useasti. S\xE4\xE4nn\xF6llisten\
  \ lausekkeiden k\xE4ytt\xF6 voi vaikuttaa suorituskykyyn, joten jos teht\xE4v\xE4\
  \ on yksinkertainen tai suoritetaan useita kertoja suuren datam\xE4\xE4r\xE4n yhteydess\xE4\
  , kehitt\xE4jien kannattaa harkita suorituskykyoptimointeja."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: - Miten:
```java
public class CharacterDeletion {

    public static void main(String[] args) {
        String input = "Puhutaanpa Javasta - Let's talk about Java!";
        String pattern = "[aeiouyäö]";

        String output = input.replaceAll(pattern, "");
        System.out.println(output);
    }
}
```
Sample output:
```
Phtnp Jvst - Lt's tlk bt Jv!
```

## Deep Dive - Syväsukellus:
Poistettaessa merkkejä Java-ohjelman merkkijonosta säännöllisten lausekkeiden avulla käytetään tyypillisesti `String`-luokan `replaceAll()`-metodia. Tämä käyttäytyminen on ollut Java-kielessä alkuajoista lähtien, jolloin säännölliset lausekkeet lisättiin Java 1.4:ään. 

Vaihtoehtoisia tapoja poistaa merkkejä ovat `replace()`-metodi (ilman regex-tukea) ja `Pattern`-luokka (suorituskykyisempää regex-käsittelyä varten). `Pattern`-luokka esikompiloi regex-kuvion, mikä on hyödyllistä, jos samaa kuviota käytetään useasti.

Säännöllisten lausekkeiden käyttö voi vaikuttaa suorituskykyyn, joten jos tehtävä on yksinkertainen tai suoritetaan useita kertoja suuren datamäärän yhteydessä, kehittäjien kannattaa harkita suorituskykyoptimointeja.

## See Also - Katso Myös:
- Java `Pattern`-luokka: https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/util/regex/Pattern.html
- Java `String`-luokan metodeja: https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/String.html
- Oracle säännölliset lausekkeet opas: https://docs.oracle.com/javase/tutorial/essential/regex/
