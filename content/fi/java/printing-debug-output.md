---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Java: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulosteiden tulostaminen on tärkeä osa ohjelmoinnin prosessia. Se auttaa ohjelmoijaa löytämään ja korjaamaan virheitä koodissa ja varmistamaan, että ohjelma toimii oikein.

## Miten

Useimmissa tapauksissa debug-tulosteita tarvitsee tulostaa vain testaustyökaluja käytettäessä, jotta voidaan seurata koodin toimintaa ja muuttujien arvoja. Tämä voidaan tehdä Java-koodissa käyttämällä System.out.println() -metodia. Esimerkiksi:

```Java
int x = 5;
System.out.println("x on nyt " + x); //tulostaa "x on nyt 5"
```

Jos haluat tulostaa tarkempia tietoja, voit myös käyttää System.out.printf() -metodia, joka antaa sinun määrittää tarkemman tulostusmuodon. Esimerkiksi:

```Java
double a = 3.1415;
System.out.printf("Arvo on %.2f", a); //tulostaa "Arvo on 3.14"
```

Voit myös käyttää Log4j-kirjastoa tulostamaan debug-tulosteita, mikä tarjoaa enemmän vaihtoehtoja ja joustavuutta debuggaamiseen.

## Syväsukellus

Debug-tulosteita tulisi käyttää vain testauksen yhteydessä ja ne tulisi poistaa ennen ohjelman julkaisemista. Liiallinen debug-tulosteiden käyttö voi hidastaa ohjelman suoritusta ja aiheuttaa turhaa tietoliikennettä.

Lisäksi on tärkeää varmistaa, että debug-tulosteet eivät sisällä arkaluonteista tai turhaa informaatiota, kuten salasanoja tai käyttäjän yksityisiä tietoja.

## Katso myös

- [Oracle:n opas debuggaamiseen Java-kielen kanssa](https://docs.oracle.com/javase/7/docs/technotes/guides/language/compiler-api.html)
- [Log4j -kirjaston viralliset sivut](https://logging.apache.org/log4j/2.x/)