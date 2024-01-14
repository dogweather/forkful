---
title:    "Java: Alimerkkijonojen eristäminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Substringien eristäminen on tärkeä osa Java-ohjelmointia, joka voi auttaa sinua käsittelemään ja muokkaamaan merkkijonoja. Se on erittäin hyödyllinen taito, joka voi säästää sinut paljon aikaa ja vaivaa koodatessasi.

## Miten tehdä

```Java
// Esimerkki 1: Merkkijonon jakaminen osiin
String s = "Tämä on esimerkki";
System.out.println(s.substring(0, 4)); // Tulostaa "Tämä"
System.out.println(s.substring(5, 7)); // Tulostaa "on"

// Esimerkki 2: Merkkijonon alku- ja loppuosien selvittäminen
String s = "Hello World";
System.out.println(s.substring(0, 5)); // Tulostaa "Hello"
System.out.println(s.substring(6)); // Tulostaa "World"
```

#### Tuloste:

```Hello
World
```

## Syvällinen sukellus

Substringien eristäminen on mahdollista Java-ohjelmoinnissa käyttämällä `substring()` -metodia, joka ottaa parametreikseen aloitus- ja lopetusindeksit. Tämä tarkoittaa, että voit aloittaa eristämisen mistä tahansa kohdasta merkkijonossa, esimerkiksi sanojen ja välilyöntien väliltä.

On myös hyvä huomata, että merkkijonojen indeksointi alkaa aina nollasta, eli ensimmäinen merkki on indeksissä 0 ja toinen indeksissä 1 jne.

## Katso myös

- [Java - String-luokka](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java - Substringien eristäminen](https://www.geeksforgeeks.org/java-string-substring-method/)
- [Java - Merkkijonon jäsentely](https://www.tutorialspoint.com/java/java_string_split.htm)