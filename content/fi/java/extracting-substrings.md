---
title:    "Java: Alimerkkijonojen erottaminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Miksi: Miksi aloittaa substringsien etsiminen Java-ohjelmoinnissa?

Substringien etsiminen on tärkeä taito Java-ohjelmoijille, sillä se mahdollistaa tietyistä merkkijonoista tiettyjen osien erottamisen. Tämä voi olla erityisen hyödyllistä esimerkiksi tekstieditorin ohjelmoinnissa tai tietokantojen käsittelyssä.

## Kuinka: Esimerkkejä substringien etsimisestä Java-koodia käyttäen

Substringien etsiminen Java-koodissa on helppoa ja tehokasta käyttäen `substring()`-metodia. Tämä metodi ottaa vastaan kaksi parametria: aloituskohdan ja lopetuskohdan. Alla on esimerkki koodista, joka etsii substringin "maailma" merkkijonosta "Hei maailma!" ja tulostaa sen konsoliin.

```Java
String sana = "Hei maailma!";
String etsittava = sana.substring(4, 10);
System.out.println(etsittava);
```
Tämän koodin tulostus on "maailma", sillä se hakee merkkijonon indeksien 4 ja 10 välisen osan.

## Syvällisempi sukellus: Lisätietoa substringien etsimisestä

Java tarjoaa myös muita metodeja substringien etsimiseen, kuten `startsWith()` ja `endsWith()`, jotka tarkistavat, alkaako tai päättyykö merkkijono tietyllä osalla. Näiden metodien käyttö voi olla hyödyllistä, kun halutaan tarkistaa, onko merkkijono esimerkiksi lueteltu tietokannassa.

On myös tärkeää ymmärtää, että merkkijonot aloittavat indeksinumeroinnin aina 0:sta, joten esimerkiksi merkkijonon "Hei" indeksinumero on 0, ei 1.

# Katso myös: Hyödyllisiä linkkejä substringien etsimisen opetteluun

- [Java substring() -dokumentaatio](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- [Java String Methods -tutoriali](https://www.w3schools.com/java/java_ref_string.asp)
- [Understanding Java Substring -artikkeli](https://www.baeldung.com/java-string-substring)
- [Java for Beginners: Substrings -opasvideo](https://www.youtube.com/watch?v=z9d7MJ2oFZI)

Kiitos lukemisesta ja toivottavasti tämä opas auttaa sinua ymmärtämään paremmin substringien etsimistä Java-ohjelmoinnissa. Onnea tuleviin ohjelmointiprojekteihisi!