---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Java: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Mitä & Miksi?
Luodessa väliaikaista tiedostoa, ohjelmoijat luovat tilapäisen tiedoston, jota voidaan käyttää ohjelman suorituksen aikana. Tämä voi olla tarpeellista esimerkiksi tiedon tallentamiseksi, väliaikaisen työn suorittamiseksi tai muussa tilanteessa, jossa tiedostoa ei haluta säilyttää pysyvästi.

Kuinka:
```Java 
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class TempFileExample {
  public static void main(String[] args) throws IOException {
    // Määritetään polku väliaikaiselle tiedostolle
    Path tempFilePath = Paths.get("C:/Users/Kayttaja/Documents/TempFile.txt");
    // Luodaan uusi väliaikainen tiedosto
    Files.createTempFile(tempFilePath, "TempFile", ".txt");
    // Tulostetaan tiedoston nimi
    System.out.println("Luotu väliaikainen tiedosto: " + tempFilePath.getFileName());
  }
}
```

Tulostus:
```
Luotu väliaikainen tiedosto: TempFile.txt
```

Syväsukellus:
Luomalla väliaikaisen tiedoston, ohjelmoijat voivat varmistaa, että heillä on tarvittavat tiedot ohjelman suorituksen aikana. Tämä menetelmä on ollut käytössä jo pitkään, ja on edelleen yksi parhaimmista tavoista käsitellä tilapäistä tietoa. Vaihtoehtoisena menetelmänä voisi olla esimerkiksi käyttää muuttujia tai tallentaa tiedot tietokantaan, mutta nämä vaihtoehdot voivat olla monimutkaisempia ja vaatia enemmän koodia.

Muut tiedostojen luomiseen liittyvät metodit, kuten File.createNewFile() tai new File() -komento, eivät luoda väliaikaista tiedostoa, vaan pysyvän. Väliaikainen tiedosto poistuu automaattisesti ohjelman suorituksen päätyttyä ja ei vie ylimääräistä tallennustilaa.

Katso myös:
Virallinen Java-tiedostojen luomiseen liittyvä dokumentaatio: https://docs.oracle.com/javase/tutorial/essential/io/file.html
Java Path-luokan dokumentaatio: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html
Tietoa tiedostojen hallinnasta Java-ohjelmoinnissa: https://www.baeldung.com/java-file-management