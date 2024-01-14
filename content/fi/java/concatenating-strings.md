---
title:                "Java: Merkkijonojen yhdistäminen"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi Java-ohjelmiston kehittäjän tulisi yhdistää merkkijonoja? Merkkijonojen yhdistäminen on erittäin kätevä tapa luoda dynaamisia ja monipuolisia viestejä ja tietokenttiä sovelluksissa. Se antaa ohjelmoijalle mahdollisuuden käyttää muuttuvia arvoja ja luoda monimutkaisempia tekstejä käyttäjien nähtäväksi. 

## Kuinka

Ohessa esimerkki miten yhdistää merkkijonoja käyttäen Java-ohjelmointikieltä:

```Java
// Luodaan kaksi merkkijonoa
String nimi = "Laura";
String tervehdys = "Hei ";


// Yhdistetään merkkijonot yhteen
String tervetuloaViesti = tervehdys + nimi;


// Tulostetaan viesti
System.out.println(tervetuloaViesti);

// Output: Hei Laura
```

Tässä esimerkissä yhdistimme kaksi merkkijonoa yhteen käyttäen plus-merkkiä (+). Toinen merkkijonoista sisälsi muuttujan, jonka arvo voitiin vaihtaa haluttaessa. Näin voimme luoda monipuolisempia viestejä tarpeen mukaan.

## Syvempi sukellus

Merkkijonojen yhdistäminen voidaan toteuttaa myös käyttäen `StringBuilder`-luokkaa, joka on tarkoitettu nimenomaan tekstin muokkaamiseen. Tämä voi olla tehokkaampi ratkaisu erityisesti silloin, kun on tarve muokata suurempia määriä tekstiä.

Tässä esimerkki miten `StringBuilder`-luokkaa voi käyttää merkkijonojen yhdistämiseen:

```Java
StringBuilder sb = new StringBuilder();

// Luodaan muuttujat
String etunimi = "Laura";
String sukunimi = "Kivinen";
int ika = 25;

// Lisätään tekstiä StringBuilder-olioon
sb.append("Hei, olen ")
  .append(etunimi)
  .append(" ")
  .append(sukunimi)
  .append(" ja olen ")
  .append(ika)
  .append(" vuotta vanha.");

// Tulostetaan lopullinen viesti
System.out.println(sb.toString());

// Output: Hei, olen Laura Kivinen ja olen 25 vuotta vanha.
```

Tässä esimerkissä yhdistimme neljä erillistä merkkijonoa yhteen käyttäen `StringBuilder`-luokkaa. Huomaa, että tällä tavalla viestin muokkaaminen ja uusien tietojen lisääminen on paljon helpompaa ja selkeämpää kuin perinteisellä plus-merkillä yhdistämällä.

## Katso myös

- [Java-merkkijonot (Oracle)](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [StringBuilder-luokka (Oracle)](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)