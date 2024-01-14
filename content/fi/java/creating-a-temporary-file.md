---
title:    "Java: Väliaikaisen tiedoston luominen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda tilapäistiedosto?

Joskus Java-ohjelmointikentässä törmätään tilanteeseen, jossa tarvitaan väliaikainen tiedosto ohjelman suorituksen aikana. Tämä voi olla esimerkiksi tarve tallentaa väliaikaista dataa, tehdä varmuuskopioita tai suorittaa muunlaisia toimintoja, jotka vaativat väliaikaisen tiedoston. Tässä blogikirjoituksessa käymme läpi, miten luodaan tilapäistiedosto Java-koodissa ja miksi se voi olla hyödyllistä.

## Miten luoda tilapäistiedosto?

Tilapäistiedoston luominen Javassa on helppoa `java.io.File`-luokan avulla. Käytämme `File.createTempFile()`-metodia luomaan tilapäistiedoston ohjelman juurihakemistoon. Alla on esimerkkikoodi, jossa luomme tilapäistiedoston nimeltä "tempfile.txt" ja kirjoitamme siihen tekstiä:

```java
File tempFile = File.createTempFile("tempfile", ".txt"); // luodaan tilapäistiedosto
try (PrintWriter writer = new PrintWriter(tempFile)) { // avaamme tiedostonkirjoitin
    writer.println("Tämä on tilapäinenteksti."); // kirjoitetaan teksti tiedostoon
    writer.flush(); // tyhjennetään kirjoitin
    System.out.println("Tilapäistiedosto luotu: " + tempFile.getAbsolutePath()); // tulostetaan tiedoston polku
}
```

Ohjelman tulostamaksi viestiksi saadaan "Tilapäistiedosto luotu: [tiedoston polku]", ja tiedoston sisällöksi "Tämä on tilapäinenteksti."

## Syvempi sukellus tilapäistiedoston luomiseen

Kun luodaan tilapäistiedosto Java-koodissa, tiedosto tallennetaan oletusarvoisesti ohjelman juurihakemistoon. Tämä voidaan kuitenkin muuttaa antamalla `createTempFile()`-metodille parametreina haluttu tiedostonimi ja hakemistopolku, johon tiedosto halutaan tallentaa.

Lisäksi, `createTempFile()`-metodille voidaan antaa myös prefix sekä suffix tiettyä formaattia varten. Tämä varmistaa, että tilapäistiedosto ei sekoitu muiden tiedostojen joukkoon.

Vaikka tilapäistiedosto poistetaan automaattisesti, kun ohjelma suoritetaan loppuun, on hyvä tapa poistaa se itse ohjelmakoodissa `deleteOnExit()`-metodia käyttäen. Tällä varmistetaan, että tilapäistiedosto ei jää turhaan roikkumaan tietokoneelle.

## Katso myös

- [Java File-luokka](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [createTempFile() Java-Docs](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)