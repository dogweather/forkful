---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "Java: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tiedostojen käsittely on keskeinen osa monien ohjelmien toimintaa, ja usein näissä ohjelmissa on tarve tarkistaa, onko tietty hakemisto olemassa. Tämä tarkistus on tärkeä varsinkin silloin, kun ohjelma käyttää hakemiston sisältöä. Tässä artikkelissa näytämme, miten tarkistaa, onko hakemisto olemassa Java-ohjelmoinnissa ja miksi tämä on tärkeää.

## Miten?
Tämän tarkistuksen suorittamiseksi käytämme File-luokan metodia "exists()", joka tarkistaa, onko annettu tiedosto tai hakemisto olemassa. Tämän jälkeen voimme käyttää "if"-lauseketta ja tulostaa halutut viestit, jos hakemisto on olemassa tai ei ole olemassa. Alla on esimerkki koodista ja sen tulostuksesta.

```Java
File hakemisto = new File("./hakemisto");

if (hakemisto.exists()) {
    System.out.println("Hakemisto on olemassa!");
} else {
    System.out.println("Hakemistoa ei löydy.");
}
```
Tulostus:
```
Hakemisto on olemassa!
```

## Syväsukellus
Tarkistus, onko hakemisto olemassa, on ollut osa Java APIa aina ensimmäisestä versiosta lähtien, joten se on vakiintunut tapa suorittaa tämä toiminto Java-ohjelmissa. Toinen tapa tarkistaa hakemiston olemassaolo on käyttää "File.isDirectory()" -metodia, joka tarkistaa, onko kyseessä hakemisto vai ei. Käyttämällä tätä metodia, voimme myös suorittaa muita toimintoja, kuten tulostaa tämän tiedon tai siirtää tiedostoja.

## Katso myös
Ohjelmoinnissa on usein tarpeen tarkistaa tiedostojen ja hakemistojen olemassaolo. Jos haluat syvempää tietoa tiedostojen käsittelystä Java-ohjelmissa, kannattaa tutustua Java API:n File-luokkaan ja sen metodeihin. Lisätietoa löytyy myös Java SE:n virallisilta verkkosivuilta: https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html