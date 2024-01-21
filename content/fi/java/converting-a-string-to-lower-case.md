---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:38:45.419134-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Muuttaa stringin pieniksi kirjaimiksi. Teemme näin muun muassa tiedon normalisointiin ja vertailuihin, jolloin isot kirjaimet eivät vaikuta tulokseen.

## How to (Kuinka tehdä):
Java tarjoaa helpon tavan muuttaa merkkijonojen kirjainkoko metodia `toLowerCase()` käyttäen. Katsotaanpa koodia.

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Hello, World!";
        String lowerCased = original.toLowerCase();
        System.out.println(lowerCased);
    }
}
```

Esimerkin tulostus:

```
hello, world!
```

## Deep Dive (Syväsukellus):
Javassa `toLowerCase()` metodin käyttäminen yleistyi jo varhaisessa vaiheessa, sillä se on peräisin String-luokasta, joka on osa Java-perusympäristöä.

* Historiallinen konteksti: Metodi on ollut olemassa Javan ensimmäisestä versiosta lähtien.
* Vaihtoehdot: Voit myös käyttää `Apache Commons Lang` -kirjastoa, `StringUtils.lowerCase()` metodilla, joka voi käsitellä `null` arvoja.
* Toteutuksen yksityiskohdat: Javan `toLowerCase()` metodi käyttää paikallisia asetuksia (locale) merkkijonoa pienentäessään. Se voi olla merkittävää kielissä, joissa on erityisiä sääntöjä kirjainkoille.

## See Also (Katso myös):
- [Java String toLowerCase() Method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html#lowerCase-java.lang.String-)