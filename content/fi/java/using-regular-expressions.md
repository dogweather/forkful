---
title:                "Java: Säännöllisten lausekkeiden käyttö"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan joutunut etsimään tietoa tekstistä, niin tiedät kuinka aikaa vievää ja työlästä se voi olla. Regular expressionit, tai regexit, tarjoavat tehokkaan tavan etsiä ja manipuloida tekstiä käyttämällä tiettyä syntaksia. Ne voivat säästää paljon aikaa ja vaivaa.

## Miten

Regular expressionit käytetään pääasiassa ohjelmointikielissä, kuten Javassa, ja ne ovat hyödyllisiä esimerkiksi tietojen validointiin ja hakemiseen tekstistä. Alla on esimerkki siitä, miten löydetään kaikki sähköpostiosoitteet annetusta tekstistä.

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

String teksti = "Osoitteeni on esimerkki@esimerkki.com ja ystäväni osoite on toinen@esimerkki.fi";
Pattern p = Pattern.compile("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}");
Matcher m = p.matcher(teksti);

while (m.find()) {
  System.out.println("Löydetty sähköpostiosoite: " + m.group());
}
```

Tulostus olisi:

```
Löydetty sähköpostiosoite: esimerkki@esimerkki.com
Löydetty sähköpostiosoite: toinen@esimerkki.fi
```

## Syväsukellus

Regular expressioneilla on paljon erilaisia syntakseja ja mahdollisuuksia, joten niiden käytön syvällisempi ymmärtäminen voi viedä aikaa ja harjoittelua. Alla on muutamia esimerkkejä erilaisista syntakseista:

- `[a-z]` vastaa mihin tahansa pieniin kirjaimiin välillä a-z
- `+` vastaa yhteen tai useampaan samaan merkkiin
- `|` vastaa joko vasemmalla tai oikealla puolella olevaan ilmaisuun

Syntaksit riippuvat käytettävästä ohjelmointikielestä ja niitä voi olla hyvä tutkia lisää esimerkiksi Java-luokkakirjastojen dokumentaatiosta.

## Katso myös

- [Regex tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Java luokkakirjasto: Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regex crossword](https://regexcrossword.com/) (hyvä tapa harjoitella regexeja!)