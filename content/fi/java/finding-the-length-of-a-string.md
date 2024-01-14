---
title:                "Java: Merkkijonon pituuden etsiminen"
simple_title:         "Merkkijonon pituuden etsiminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan ensimmäistä blogikirjoitustani Java-ohjelmoinnista! Tämän kirjoituksen aiheena on merkkijonon pituuden laskeminen, eli kuinka monta merkkiä merkkijonossa on. Tämä taito on olennainen Java-ohjelmoijalle, ja se auttaa sinua työskentelemään merkkijonojen kanssa tehokkaasti.

## Kuinka tehdä

Java tarjoaa meille valmiin metodin, jolla voimme helposti laskea merkkijonon pituuden. Tämän metodin nimi on `.length()` ja se palauttaa kokonaislukuarvon merkkijonon pituudesta. Katsotaanpa esimerkiksi miten tämä toimii käytännössä:

```java
public static void main(String[] args) {
	String s = "Tervetuloa";
	System.out.println("Merkkijonon pituus on: " + s.length());
}
```
**Output:**
```
Merkkijonon pituus on: 10
```

Hienoa, näemme että merkkijonon `.length()` metodi palauttaa oikean tuloksen! Huomaa, että kun tulostamme merkkijonon pituuden, meidän tulee käyttää sulkumerkkejä `()` metodin lopussa osoittamaan että kyseessä on metodi.

## Syväsukellus

Monilla muilla ohjelmointikielillä merkkijonon pituuden laskeminen tapahtuu eri tavalla, esimerkiksi C-kielelle löytyy oma funktio `.strlen()`. Java-ohjelmointikielen suosio on kasvanut pitkälti sen vuoksi, että se tarjoaa selkeämmän ja yksinkertaisemman tavan käsitellä mm. merkkijonoja.

Merkkijonojen pituuden laskeminen on tärkeä taito ohjelmoinnissa, sillä se auttaa meitä esimerkiksi tarkistamaan että käyttäjän antama syöte on oikean mittainen ja samalla se voi auttaa meitä käsittelemään merkkijonoja tarkemmin. Jos olet kiinnostunut oppimaan lisää merkkijonojen käsittelystä Java-ohjelmoinnissa, suosittelen tutustumaan Java-kirjaston `String` luokkaan.

## Katso myös

- [Java String-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Lyhyt oppimäärä Java-ohjelmointiin](https://www.tutorialspoint.com/java/index.htm)
- [Java Tutorials - String Methods](https://www.javatpoint.com/string-methods)