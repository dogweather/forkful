---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- fi/java/searching-and-replacing-text.md
date:                  2024-01-20T17:58:06.109199-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
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
