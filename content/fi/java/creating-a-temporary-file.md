---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Luodaan väliaikainen tiedosto, kun tarvitsemme tiedostoa, mutta ei haluta, että se jää pysyvasti tietokoneellemme. Tämä on hyödyllistä, kun käsittelemme suuria tietomääriä, jotka haluamme kertakäyttöisiksi tai kun testaamme ohjelmistoamme.

## Miten:

Java tarjoaa `File`-luokkaa väliaikaisten tiedostojen luomiseen. Katsotaan esimerkkiä:

```Java
import java.io.File;
import java.io.IOException;

public class Main {
  public static void main(String[] args) {

    // Luodaan väliaikainen tiedosto
    try {
      File tempFile = File.createTempFile("testi", ".txt");

      // Tulostetaan tiedoston polku
      System.out.println("Väliaikaista tiedostoa luotu: " + tempFile.getAbsolutePath());

      // Tiedosto poistetaan, kun JVM sulkeutuu
      tempFile.deleteOnExit();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```
Tämä luo uuden väliaikaisen tiedoston, tulostaen sen tiedoston polun ja poistaen sen, kun JVM sulkeutuu. Käytämme `deleteOnExit()`-metodia varmistamaan, että tiedosto poistetaan, kun ohjelma päättyy.

## Deep Dive:

Väliaikaisten tiedostojen käyttö ei ole uusi käsite - ohjelmoijat ovat luoneet niitä aina tarvittaessa, varsinkin suurissa datan käsittelyprosesseissa.

Jos olet törmännyt tilanteeseen, jossa `File.createTempFile()` ei ole tarpeeksi, harkitse java.nio.file -paketin `Files.createTempDirectory()`-metodia, joka luo väliaikaista hakemistoa.

Java luo väliaikaiset tiedostot käyttäjän kotihakemiston alihakemistoon. Voit muuttaa oletushakemistoa asettamalla `java.io.tmpdir`-järjestelmäominaisuuden.

## Katso Myös:

1. [Oracle Java File Documentation](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html)
2. [Oracle Java IO tutorial](https://docs.oracle.com/javase/tutorial/essential/io/)