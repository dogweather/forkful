---
title:    "Java: Mallia vastaavien merkkien poistaminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Joissakin tilanteissa on tarpeen poistaa tietyn kuvion mukaiset merkit tekstin joukosta. Tämä voi vaikuttaa vaivalloiselta tai aikaa vievältä tehtävältä, mutta onneksi Java-ohjelmoinnissa on käytettävissä tehokas tapa automatisoida tämä prosessi.

## Kuinka tehdä

Javan String-luokka tarjoaa useita hyödyllisiä metodeja merkkijonojen manipulointiin. Yksi näistä on `replaceAll()`-metodi, joka ottaa ensimmäisenä parametrinaan säännöllisen lausekkeen ja toisena parametrina korvaavan merkkijonon. Alla on esimerkki koodista, joka poistaa kaikki numerot merkkijonosta:

```Java
String teksti = "ABC123DEF456";
teksti = teksti.replaceAll("[0-9]", "");
System.out.println(teksti);
```

Tulostus olisi seuraava:

```Java
ABCDEF
```

Tässä esimerkissä `[0-9]` on säännöllinen lauseke, joka vastaa kaikkia numeromerkkejä. `""` taas on tyhjä merkkijono, joka toimii korvaavana merkkijonona.

## Syvempi sukellus

Säännölliset lausekkeet ovat tehokas työkalu merkkijonojen hallintaan. Niiden avulla voit tarkasti määritellä minkä tahansa kaavion tai kuviomerkinnän ja poistaa sen tarvittaessa. Voit myös käyttää erilaisia metakaraktereja, kuten `.` tai `*`, joiden avulla voit tehdä entistä monimutkaisempia kuvioita vastaavia hakuja.

On myös hyvä pitää mielessä, että `replaceAll()`-metodi palauttaa uuden merkkijonon, joten alkuperäinen merkkijono säilyy muuttumattomana. Jos haluat muuttaa alkuperäistä merkkijonoa, voit käyttää `replace()`-metodia, joka toimii samalla tavalla, mutta muuttaa suoraan alkuperäistä merkkijonoa tarvittaessa.

## Katso myös

- [Java String-luokka](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Säännölliset lausekkeet](https://www.regular-expressions.info/)
- [replaceAll() vs. replace()](https://stackoverflow.com/questions/5881409/string-replace-vs-string-replaceall)