---
title:                "Tiedostotiedon kirjoittaminen"
html_title:           "Java: Tiedostotiedon kirjoittaminen"
simple_title:         "Tiedostotiedon kirjoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tekstitiedoston kirjoittaminen on yksinkertainen tapa tallentaa tietoja tietokoneelle. Ohjelmoijat käyttävät sitä usein tallentaakseen muuttuvia tietoja esimerkiksi käyttäjän asetuksista tai tuloksista.

## Kuinka:
Java:lla voit kirjoittaa tekstitiedoston helposti käyttämällä FileWriter-luokkaa. Katso alla oleva koodiesimerkki ja tulosteet.

```java
// Luodaan FileWriter-olio ja tiedoston nimi
FileWriter tiedosto = new FileWriter("tiedostonimi.txt");

// Kirjoitetaan tiedostoon tekstiä
tiedosto.write("Tämä on esimerkki tekstistä!");

// Suljetaan tiedosto
tiedosto.close();
```

Tämän jälkeen voit tarkistaa tekstitiedostosi ja nähdä, että se sisältää haluamasi tekstin.

## Syväsukellus:
Tekstitiedoston kirjoittaminen on ollut käytössä ohjelmoinnissa jo pitkään, ja se on edelleen erittäin hyödyllinen tapa tallentaa tietoja. Joissakin tapauksissa voit myös käyttää muita tiedostomuotoja, kuten CSV tai XML, jos haluat tallentaa monimutkaisempia tietoja. On myös tärkeää pitää mielessä, että tekstitiedostot ovat yleensä hyvin pienikokoisia ja niitä on helppo jakaa ja siirrellä.

## Katso myös:
- https://docs.oracle.com/javase/tutorial/essential/io/file.html (Java:n virallinen dokumentaatio tiedostojen kirjoittamisesta)
- https://www.tutorialspoint.com/java/java_files_io.htm (opastus tietojen tallentamisesta Java:lla)