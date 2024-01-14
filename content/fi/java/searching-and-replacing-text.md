---
title:                "Java: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Tekstin etsimisessä ja korvaamisessa on hyötyä, jos haluat nopeasti ja tehokkaasti muuttaa suuren määrän tekstiä. Tämä voi olla hyödyllistä esimerkiksi, kun haluat korjata useita kirjoitusvirheitä tai päivittää tiettyjä termejä projekteissasi.

## Kuinka
Java tarjoaa useita tapoja etsiä ja korvata tekstiä eri tavoilla. Yksinkertaisin tapa on käyttää String-luokan replace()-metodia, joka korvaa kaikki annetut merkkijonot toisella merkkijonolla. Esimerkiksi seuraava koodinpätkä korvaa sanan "tunti" sanalla "minuutti":

```Java
String teksti = "Hän vietti kaksi tuntia harjoitellen soittamista.";
String uusiTeksti = teksti.replace("tunti", "minuutti");
System.out.println(uusiTeksti);
```

Tulostus:
```
Hän vietti kaksi minuuttia harjoitellen soittamista.
```

Jos haluat vaihtaa vain tietyn kohdan tekstissä, voit käyttää replaceFirst()-metodia. Se korvaa vain ensimmäisen annetun merkkijonon. Alla olevassa esimerkissä vaihdetaan ensimmäinen tapaus sanasta "kissa" sanaan "koira":

```Java
String lause = "Minulla on kolme kissaa ja yksi koira.";
String uusiLause = lause.replaceFirst("kissa", "koira");
System.out.println(uusiLause);
```

Tulostus:
```
Minulla on kolme koiraa ja yksi koira.
```

Jos haluat vaihtaa vain tietyn osan tekstistä, voit käyttää replace() -metodia antamalla sen alku- ja loppuindeksit. Se korvaa vain annetun alueen:

```Java
String teksti = "Tämä on esimerkki tekstistä.";
String uusiTeksti = teksti.replace( 10, 20, "hieno" );
System.out.println(uusiTeksti);
```

Tulostus:
```
Tämä on hieno esimerkki tekstistä.
```

## Syvällisempi sukellus
Java tarjoaa myös monia muita hyödyllisiä metodeja tekstin etsimiseen ja korvaamiseen. Voit käyttää String-luokan matches()-metodia tarkistaaksesi, vastaako teksti tiettyä säännöllistä lausetta. Tai käytä split()-metodia pilkkoaksesi teksti haluamiisi osiin.

Jos haluat käsitellä tekstejä suuremmassa mittakaavassa, voit käyttää RegEx (Regular Expression) -kirjastoa. Se tarjoaa monipuolisia työkaluja tekstin käsittelyyn, mukaan lukien tekstien etsiminen ja korvaaminen erittäin monimutkaisilla säännöillä.

## Katso myös
- [Java String-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Regular Expressions -tutoriaali](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Java String.replace() vs. String.replaceAll()](https://www.baeldung.com/java-string-replace-replaceall)