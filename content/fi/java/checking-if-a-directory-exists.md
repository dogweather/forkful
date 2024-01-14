---
title:                "Java: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On olemassa monia tilanteita, joissa sinun voi olla tarpeen tarkistaa, onko hakemisto olemassa. Esimerkiksi haluat ehkä tarkistaa, onko tiedosto tallennettu tietokoneellesi ennen kuin yrität avata ja käsitellä sitä.

## Miten

```java
import java.io.File;

public class Hakemisto {

    public static void main(String[] args) {

        // Luo uusi tiedosto-olio
        File tiedosto = new File("polku/hakemisto");

        // Tarkista onko hakemisto olemassa
        if (tiedosto.isDirectory()) {
            System.out.println("Hakemisto on olemassa!");
        } else {
            System.out.println("Hakemistoa ei löytynyt.");
        }
    }
}
```

Koodinpätkässä luodaan uusi tiedosto-olio, joka osoittaa tiettyyn hakemistoon. Sitten tarkistetaan, onko tätä hakemistoa olemassa käyttämällä `isDirectory()` -metodia. Jos hakemisto on olemassa, tulostetaan viesti "Hakemisto on olemassa!", muuten tulostetaan viesti "Hakemistoa ei löytynyt."

## Syväkuopat

On tärkeää huomata, että tarkistaessaan hakemiston olemassaoloa, ohjelma tarkistaa vain, onko hakemisto olemassa. Se ei tarkista, onko tiedostopolku oikein tai onko kyseessä todellinen hakemisto. Tämä voi johtaa virheelliseen tulokseen, jos esimerkiksi annat väärän hakemiston nimen.

Toinen asia, joka kannattaa huomata, on että `isDirectory()` -metodi palauttaa `false`, jos tarkistettava tiedosto on olemassa, mutta se ei ole hakemisto. Tässä tapauksessa kannattaa käyttää `isFile()` -metodia tarkistaaksesi, onko kyseessä tiedosto.

## Katso myös

- java.io.File - Java API Documentation: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- How to check if directory exists using Java: https://www.baeldung.com/java-check-directory-exists