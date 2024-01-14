---
title:    "Java: Merkkijonojen yhdisteleminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja Java-ohjelmoinnissa? Merkkijonon yhdistäminen on kätevä tapa luoda yksi kokonainen teksti useista osista. Tämä voi olla erityisen hyödyllistä, kun haluat rakentaa dynaamisia käyttöliittymiä tai tarkentaa tulosteita. Merkkijonojen yhdistäminen myös auttaa pitämään koodin siistinä ja helposti luettavana.

## Kuinka

Javassa merkkijonojen yhdistäminen tapahtuu käyttämällä plus-merkkiä (+) tai concat()-metodia. Kumpikin vaihtoehto tuottaa saman lopputuloksen, mutta concat()-metodi on hieman tehokkaampi.

```Java
// Plus-merkki (+)
String nimi = "Maija";
String tervehdys = "Hei " + nimi + "!";

// concat()-metodi
String nimi = "Maija";
String tervehdys = "Hei ".concat(nimi).concat("!");
```

Tässä esimerkissä olemme yhdistäneet merkkijonot "Hei ", "Maija" ja "!". Lopputuloksena saamme merkkijonon "Hei Maija!".

## Syvällinen sukellus

Merkkijonojen yhdistäminen voi olla tehokasta, mutta on myös tärkeää pitää mielessä muutama asia. Ensinnäkin, on tärkeää muistaa, että merkkijonot ovat pysyviä ja niitä ei voi muuttaa. Siksi joka kerta kun yhdistämme merkkijonon, luodaan uusi merkkijono-objekti. Tämä voi aiheuttaa suorituskykyongelmia, jos käytämme merkkijonojen yhdistämistä suurissa määrin.

On myös tärkeää huomata, että merkkijonojen yhdistäminen voi olla hidas prosessi. Tämä johtuu siitä, että Javan String-luokkaa ei ole suunniteltu käsittelemään suuria merkkijonoja tehokkaasti. Jos tarvitset suurien merkkijonojen käsittelyä, harkitse StringBuilder- tai StringBuffer-luokkia.

## Katso myös

- [Javan String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Javan StringBuilder-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Javan StringBuffer-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)