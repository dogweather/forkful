---
title:                "Merkkijonon isoittaminen"
html_title:           "Java: Merkkijonon isoittaminen"
simple_title:         "Merkkijonon isoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoijan täytyy muuttaa merkkijonon kirjaimet isolla alkukirjaimella alkaviksi. Tässä artikkelissa näytämme, miten se voidaan tehdä helposti Java-kielellä.

## Miten

Käyttäen String-luokan metodia "toUpperCase()", voimme muuttaa merkkijonon kaikki kirjaimet isoiksi. Tämän jälkeen voimme käyttää "substring()" metodia, jotta saadaan alkukirjain isolla mutta loput kirjaimet pienillä. Alla olevassa esimerkissä oletetaan, että annettu merkkijono on "hello world".

```java
String s = "hello world";
s = s.toUpperCase();
s = s.substring(0, 1) + s.substring(1).toLowerCase();
System.out.println(s); // Tulostaa "Hello world"
```

## Syvempää tietoa

Java-kielellä on myös muita tapoja muuttaa merkkijonon kirjaimia isolla alkukirjaimella. Voimme esimerkiksi käyttää "split()" ja "join()" metodeja yhdistettynä "toUpperCase()" ja "toLowerCase()" metodeihin. Tämä voisi näyttää seuraavalta:

```java
String s = "hello world";
String[] parts = s.split(" ");
for (int i = 0; i < parts.length; i++) {
  parts[i] = parts[i].substring(0, 1).toUpperCase() + parts[i].substring(1).toLowerCase();
}
s = String.join(" ", parts);
System.out.println(s); // Tulostaa "Hello World"
```

## Katso myös

- [String-luokka Java API:ssa](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- ["toUppercase()" Java API:ssa](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- ["substring()" Java API:ssa](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)