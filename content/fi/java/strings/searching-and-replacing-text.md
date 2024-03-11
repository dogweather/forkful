---
date: 2024-01-20 17:58:06.109199-07:00
description: "Tekstin etsiminen ja korvaaminen on tapa l\xF6yt\xE4\xE4 merkkijono\
  \ ja korvata se toisella. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 p\xE4\xE4st\xE4\
  kseen eroon vanhentuneesta koodista,\u2026"
lastmod: '2024-03-11T00:14:30.370357-06:00'
model: gpt-4-1106-preview
summary: "Tekstin etsiminen ja korvaaminen on tapa l\xF6yt\xE4\xE4 merkkijono ja korvata\
  \ se toisella. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 p\xE4\xE4st\xE4kseen eroon\
  \ vanhentuneesta koodista,\u2026"
title: Tekstin etsiminen ja korvaaminen
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin etsiminen ja korvaaminen on tapa löytää merkkijono ja korvata se toisella. Ohjelmoijat käyttävät sitä päästäkseen eroon vanhentuneesta koodista, päivittääkseen tietoja tai tehdäkseen massamuutoksia nopeasti.

## Miten:

Java tarjoaa `String`-luokan, jossa on metodit `replace()` ja `replaceAll()` tekstinkäsittelyyn. `replace()` toimii merkeille ja merkkijonoille, `replaceAll()` säännöllisille lausekkeille.

```java
public class StringReplaceExample {
    public static void main(String[] args) {
        String originalString = "Ohjelmointi on hauskaa, kunnes ei ole.";
        String replacedString = originalString.replace("hauskaa", "hidasta");
        System.out.println(replacedString);

        String regexReplacedString = originalString.replaceAll("hauskaa|ei ole", "turhauttavaa");
        System.out.println(regexReplacedString);
    }
}
```

Tulostus:

```
Ohjelmointi on hidasta, kunnes ei ole.
Ohjelmointi on turhauttavaa, kunnes turhauttavaa.
```

## Syväsukellus:

Historiallisesti tekstin korvaaminen tiedostoissa tehtiin komentorivillä käyttäen työkaluja kuten `sed` tai `awk` Unix-pohjaisissa järjestelmissä. Java-toteutus on osa korkean tason API:a, joka tekee käsittelystä yksinkertaista. `replaceAll()` käyttää säännöllisiä lausekkeita, jotka ovat voimakas mutta monimutkainen työkalu tekstinkäsittelyyn. Tämä on nopeampaa ja joustavampaa kuin manuaalisesti kirjoitetut ratkaisut.

## Katso Myös:

- Java String Documentation: https://docs.oracle.com/javase/10/docs/api/java/lang/String.html
- RegExr, säännöllisten lausekkeiden harjoitteluun: https://regexr.com/
- `Pattern` ja `Matcher` luokat: https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html
