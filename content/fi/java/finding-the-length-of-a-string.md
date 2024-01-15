---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Java: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Kannattaako vaivan nähdä selvittää merkkijonon pituus? Lyhyesti: jos haluat ohjelmasi käsittelevän syötteitä joustavasti ja tehokkaasti, on tärkeää tietää, kuinka monta merkkiä siinä on.

## Kuinka

Kun haluat selvittää merkkijonon pituuden Java-ohjelmassa, voit käyttää String-luokan length-metodia. Tämä palauttaa merkkijonon pituuden kokonaislukuna. Seuraavassa esimerkissä käytämme length-metodia ja tulostamme sen palauttaman arvon:

```Java
String s = "Hei maailma!";
System.out.println(s.length());

// Output: 12
```

Merkkijonon pituudesta on myös saatavilla muita hyödyllisiä tietoja. Esimerkiksi voit tarkistaa, onko merkkijono tyhjä käyttämällä isEmpty-metodia, joka palauttaa boolean-arvon. Seuraavassa esimerkissä tarkistamme, onko merkkijono tyhjä:

```Java
String s = "";
System.out.println(s.isEmpty());

// Output: true
```

Voit myös käyttää trim-metodia poistamaan merkkijonosta ylimääräiset välilyönnit ennen pituuden laskemista. Seuraava esimerkki näyttää, kuinka trim-metodia käytetään:

```Java
String s = "     Tämä on merkkijono, joka sisältää ylimääräisiä välilyöntejä.       ";
System.out.println(s.trim().length());

// Output: 55
```

## Syventävä tarkastelu

Merkkijonon pituuden selvittäminen on tärkeä konsepti Java-ohjelmoinnissa, sillä merkkijonoja käytetään usein syötteinä ja tulosteina. Kun tiedät merkkijonon pituuden, voit esimerkiksi tarkistaa, kuinka monta merkkiä käyttäjän syötteessä on ja tehdä sen perusteella erilaisia toimenpiteitä. Lisäksi monimutkaisissa ohjelmissa voi olla tarpeen käsitellä erilaisia merkkijonoja, ja niiden pituuden selvittäminen auttaa tehokkaassa koodin kirjoituksessa.

## Katso myös

- [Java String Class](https://www.w3schools.com/java/java_ref_string.asp)
- [Java String Methods](https://www.javatpoint.com/java-string-methods)
- [Oracle Java Docs - String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)

Lopetetaan artikkeli tähän, lyhyenä ja ytimekkäänä. Toivottavasti tämä auttoi sinua ymmärtämään merkkijonon pituuden laskemisen tärkeyden Java-ohjelmoinnissa. Hyvää ohjelmointia!