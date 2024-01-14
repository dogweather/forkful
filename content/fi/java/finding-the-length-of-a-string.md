---
title:                "Java: Merkkijonon pituuden löytäminen"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi haluaisit löytää merkkijonon pituuden? Monissa ohjelmointitehtävissä on tarpeen tietää merkkijonon pituus, jotta sen käsittely olisi mahdollista. Merkkijonon pituuden etsiminen on siis tärkeä taito, jota jokaisen Java-ohjelmoijan tulisi hallita.

## Miten?

Käyttämällä Javan String-luokan length-metodia voit löytää helposti merkkijonon pituuden. Alla on esimerkki koodista ja siihen liittyvä tulostus:

```Java
// Luo muuttuja merkkijonolle ja sijoittaa siihen teksti "Hei"
String merkkijono = "Hei";

// Käytä length-metodia saadaksesi merkkijonon pituuden ja tallenna se muuttujaan
int pituus = merkkijono.length();

// Tulosta tulos
System.out.println("Merkkijonon pituus on: " + pituus);

// Tulostus: "Merkkijonon pituus on: 3"
```

## Syventyvä tarkastelu

Merkkijonojen pituuden laskeminen perustuu niiden sisältämiin merkkeihin. Javan String-luokka tallentaa merkkijonot merkkijonotaulukkona, jota kutsutaan myös merkkipuskuriksi. Tämän taulukon avulla ohjelma pystyy laskemaan merkkijonon pituuden length-metodin avulla.

Oletusarvoisesti String-luokka käyttää UTF-16-koodausta, jossa jokainen kirjain tai merkki vie 16 bittiä muistissa. Tästä johtuen myös merkkijonon pituus lasketaan merkkijonotaulukon pituudeksi.

On myös tärkeää huomata, että length-metodi palauttaa merkkijonon todellisen pituuden, eli ilman mahdollisia tyhjiä välilyöntejä.

## Katso myös

- [Javan String-luokka ja sen toiminnot (Tietokoneturvallisuus-opas)](https://tietokoneturvallisuus.fi/ohjeet/java-string/)
- [Merkkijonon käsittely Javassa (Tietokone osaamista.io)](https://www.tietokoneosaamista.io/merkkijonojava/ominaisuudet/)
- [Java String-luokan JavaDoc (Oracle)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)