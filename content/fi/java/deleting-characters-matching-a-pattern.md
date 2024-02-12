---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
aliases:
- fi/java/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:33.317475-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Java-ohjelmoinnissa merkkien poistaminen kuvioita vastaavasti tarkoittaa tietyn säännöllisen lausekkeen (regex) määrittämiä merkkejä olevien kohtien poistamista merkkijonosta. Tätä tehdään yleensä syötteen siistimiseen, datan muotoiluun tai tarpeettoman tiedon suodattamiseen.

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
