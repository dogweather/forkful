---
aliases:
- /fi/java/finding-the-length-of-a-string/
date: 2024-01-20 17:47:43.190090-07:00
description: "Mit\xE4 ja miksi? Stringin pituuden selvitt\xE4minen tarkoittaa merkkijonon\
  \ merkkien m\xE4\xE4r\xE4n laskemista. Ohjelmoijat tarvitsevat t\xE4t\xE4 toimintoa\
  \ esimerkiksi\u2026"
lastmod: 2024-02-18 23:09:07.448524
model: gpt-4-1106-preview
summary: "Mit\xE4 ja miksi? Stringin pituuden selvitt\xE4minen tarkoittaa merkkijonon\
  \ merkkien m\xE4\xE4r\xE4n laskemista. Ohjelmoijat tarvitsevat t\xE4t\xE4 toimintoa\
  \ esimerkiksi\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Stringin pituuden selvittäminen tarkoittaa merkkijonon merkkien määrän laskemista. Ohjelmoijat tarvitsevat tätä toimintoa esimerkiksi silloin, kun haluavat varmistaa, että syöte on tietyn mittainen tai kun he pilkkovat ja käsittelevät tekstiä.

## How to:
Koodiesimerkit ja tulosteet

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String aString = "Heippa!";
        int length = aString.length();
        System.out.println("String length: " + length);
    }
}
```

Tuloste:

```
String length: 7
```

## Deep Dive
Sukellus syvyyksiin: `String`-luokan `length()`-metodi on ollut osa Javaa sen alkuajoista lähtien ja se on perusväline merkkijonojen pituuksien hallintaan. Metodi palauttaa `int`-tyypin arvon, joka kertoo merkkijonon merkkien määrän. Vaihtoehtoisia tapoja pituuden mittaamiseen ei juurikaan ole, sillä `length()` on niin yksinkertainen ja suoraan asiaan menevä. Se kannattaa kuitenkin muistaa, että tyhjän merkkijonon ("") pituus on 0 ja että `null`-arvoisella string-olioilla `length()`-metodia ei voi kutsua ilman, että ohjelma kaatuu `NullPointerException`-poikkeuksen vuoksi.

## See Also
Lisätietoja:

- Oracle Java Documentation: [String.length() method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#length())
- Java String handling tutorial: [Working with Strings in Java](https://www.baeldung.com/java-string)
- Java programming practices and principles: [Effective Java by Joshua Bloch](https://www.pearson.com/us/higher-education/program/Bloch-Effective-Java-3rd-Edition/PGM334834.html)
