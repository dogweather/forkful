---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Java: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressionit ovat hyödyllisiä työkaluja, joita voi käyttää merkkijonojen etsimiseen ja manipulointiin Java-ohjelmoinnissa. Ne mahdollistavat monimutkaisten haku- ja korvausoperaatioiden suorittamisen vain muutamalla rivillä koodia.

## Kuinka

Regular expressionit luodaan käyttämällä Java:n valmiita luokkia, kuten `Pattern` ja `Matcher`. Alla on esimerkki, jossa etsitään merkkijonosta kaikki numerot ja tulostetaan ne näytölle.

```Java
String teksti = "Tämä on esimerkki123 merkkijonosta.";
Pattern p = Pattern.compile("\\d+");
Matcher m = p.matcher(teksti);
while (m.find()) {
	System.out.println(m.group());
}
```

Tämä tulostaa seuraavan:

```
123
```

Regular expressionit käyttävät erityisiä merkkejä ja lausekkeita, joiden avulla voidaan määritellä halutunlainen haku. Esimerkiksi `\d` tarkoittaa numeromerkkiä ja `+` tarkoittaa, että merkkiä voi olla yhdestä useaan kertaan peräkkäin.

## Syvemmälle

Regular expressioneilla on laaja valikoima erilaisia merkkejä ja lausekkeita, joilla voidaan määrittää tarkkoja hakuja ja korvauksia. Esimerkiksi seuraavat merkit ovat yleisesti käytössä:

- `\d` tarkoittaa numeromerkkiä
- `\w` tarkoittaa kirjainta, numeroa tai alaviivaa
- `\s` tarkoittaa välilyöntiä tai muuta tyhjää merkkiä
- `.` tarkoittaa mitä tahansa merkkiä
- `[]` luodaan merkkijonoja, joihin matches-parametrilla täsmätä
- `^` merkki tarkoittaa merkkijonon alkuosaa
- `$` merkki tarkoittaa merkkijonon loppuosaa
- `()` luodaan ryhmiä, joihin tiedon voidaan tallentaa ja käsitellä sen jälkeen

On hyödyllistä tutustua tarkemmin erilaisiin syntaksiin ja mahdollisuuksiin, jotta voi luoda tehokkaita ja tarkkoja regular expressioneita ohjelmoinnissa.

## Katso myös

- [Oracle:n Regular Expressions -dokumentaatio (englanniksi)](https://docs.oracle.com/javase/10/docs/api/java/util/regex/package-summary.html)
- [Regular expressions -w3schools (englanniksi)](https://www.w3schools.com/java/java_regex.asp)
- [RegExr - online regular expressions editor (englanniksi)](https://regexr.com/)