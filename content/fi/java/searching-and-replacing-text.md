---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin hakeminen ja korvaaminen on prosessi, jolla etsitään joukko merkkejä (eli merkkijono) tekstistä ja korvataan ne toisella merkkijonolla. Ohjelmoijat tekevät tämän datan siivoamiseen, tiedostojen muokkaamiseen, tiedon uudelleenjärjestämiseen ja paljon muuta.

## Kuinka:

Katsotaanpa, kuinka tämä toimii Java:ssa.

```Java
public class Main {
  public static void main(String[] args) {
    String str = "Hei Maailma!";
    String updatedStr = str.replace("Hei", "Terve");
    System.out.println(updatedStr);
  }
}
```

Tässä yksinkertaisessa esimerkissä "Hei" korvataan sanalla "Terve". Outputtina saamme:

```
Terve Maailma!
```

## Syvällinen sukellus:

1. Historiallinen yhteys: Vanhat ohjelmointikielet, kuten C ja Fortran, joissa ei ollut sisäänrakennettuja funktioita tekstin hakemiseen ja korvaamiseen, vaativat ohjelmoijia kirjoittamaan paljon koodia tähän toimintoon.
2. Vaihtoehdot: Joissain kielissä, kuten Perl:ssä ja Python:ssa, on kyky suorittaa Regular Expressions (säännölliset lausekkeet), joka on tehokas tapa etsiä ja korvata tekstiä.
3. Toteutustiedot: Java:ssa `replace()`-metodi on osa `String`-luokkaa, ja se palauttaa uuden merkkijonon. Joissakin tapauksissa, kun suoritetaan useita korvaavia toimenpiteitä, esimerkiksi `StringBuilder` tai `StringBuffer` voi olla tehokkaampi.

## Katso myös:

1. Oracle Java Documentation about `replace()`: [Oracle Java Docs](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
2. Tutorial on Regular Expressions in Java: [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
3. Information on StringBuilder: [StringBuilder in Java](https://www.javatpoint.com/StringBuffer-class)