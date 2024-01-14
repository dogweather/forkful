---
title:    "Java: Tarkistetaan, löytyykö hakemistoa"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

On olemassa monia tilanteita, joissa Java-ohjelmoijan täytyy tarkistaa, onko kansio olemassa. Esimerkiksi käyttäjä voi syöttää polun tietyn kansioon ja ohjelman täytyy tarkistaa, onko kyseinen kansio olemassa ennen kuin se suorittaa sitä liittyviä toimintoja.

## Miten

Tarkistaminen, onko kansio olemassa, tapahtuu helposti Java-koodilla. Ensimmäinen askel on käyttää File-luokkaa, joka edustaa tiedostojärjestelmässä olevia tiedostoja ja kansioita. Tämän jälkeen voimme käyttää File-luokan `exists()` -metodia, joka tarkistaa, onko kansio olemassa.

```
Java
File kansio = new File("polku/kansioon");
if (kansio.exists()) {
    System.out.println("Kansio on olemassa.");
} else {
    System.out.println("Kansiota ei ole olemassa.");
}
```

Jos kansio on olemassa, konsoliin tulostetaan "Kansio on olemassa." Jos kansioa ei ole olemassa, tulostetaan "Kansiota ei ole olemassa.". Tämä on yksinkertainen tapa tarkistaa, onko kansio olemassa Java-ohjelmassa.

## Syventävä tarkastelu

Tämä menetelmä tarkistaa vain, onko kansio olemassa. Jos haluat myös varmistaa, että kyseessä on kansio eikä esimerkiksi tiedosto, voit käyttää File-luokan `isDirectory()` -metodia. Tämä palauttaa boolean-arvon, joka ilmaisee, onko kyseessä kansio vai ei.

On myös tärkeää huomata, että `exists()` ja `isDirectory()` -metodit voivat palauttaa `false` -arvon, jos käyttäjällä ei ole oikeuksia tiettyyn kansioon. Tässä tapauksessa voit käyttää `canRead()` ja `canWrite()` -metodeita tarkistaaksesi, onko käyttäjällä oikeudet lukea tai kirjoittaa kyseiseen kansioon.

## Katso myös

- [Oracle Java -tiedostojen ja kansioiden hallinta](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [Java File-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tietoa tiedostojen ja kansioiden käsittelystä Javassa](https://javabeginnerstutorial.com/core-java-tutorial/file-handling/)