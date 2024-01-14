---
title:                "Java: Väliaikaisen tiedoston luominen"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi: Miksi luoda väliaikainen tiedosto Java-ohjelmoinnissa?

Java-ohjelmoijat saattavat joutua luomaan väliaikaisia tiedostoja erilaisissa tilanteissa, kuten tallentaessaan väliaikaista dataa tai suorittaessaan tiettyjä algoritmeja. Väliaikaiset tiedostot ovat hyödyllisiä väliaikaisina säilöinä ja voidaan poistaa käytön jälkeen, mikä auttaa välttämään turhia tietokoneen resurssien käyttöä.

## Miten tehdä: Esimerkkejä Java-koodilla ja näytöllä

```Java
import java.io.File;
import java.io.IOException;

public class TemporaryFileExample {
    public static void main(String[] args) {
        try {
            // Luodaan väliaikainen tiedosto aloituspolkuun
            File tempFile = File.createTempFile("temp", ".txt");
            System.out.println("Väliaikainen tiedosto luotu: " + tempFile.getAbsolutePath());

            // Kirjoitetaan tiedostoon dataa
            PrintWriter writer = new PrintWriter(tempFile);
            writer.println("Hei, tämä on väliaikainen tiedosto!");
            writer.close();

            // Luetaan tiedoston sisältö ja tulostetaan se näytölle
            Scanner scanner = new Scanner(tempFile);
            while (scanner.hasNext()) {
                System.out.println(scanner.nextLine());
            }
            scanner.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

**Näyttö:**

```
Väliaikainen tiedosto luotu: C:\Users\Käyttäjä\AppData\Local\Temp\temp2104542262852852084.txt
Hei, tämä on väliaikainen tiedosto!
```

## Syvällinen sukellus: Lisätietoa väliaikaisten tiedostojen luomisesta

Väliaikaisten tiedostojen luominen Java-ohjelmassa tapahtuu File-luokan avulla. Tämä luokka tarjoaa useita metodeja, kuten `createTempFile()`, jotta voimme luoda väliaikaisen tiedoston haluttuun paikkaan. Oletuksena väliaikaiset tiedostot tallennetaan käyttöjärjestelmän temp-hakemistoon.

Voimme myös antaa parametrina tiedostolle halutun nimen ja tiedostopäätteen, kuten `.txt`, jolloin voimme helposti tunnistaa ja käsitellä väliaikaista tiedostoa. Väliaikainen tiedosto poistetaan automaattisesti, kun ohjelma suoritus päättyy tai kun se poistetaan manuaalisesti `delete()`-metodilla.

# Katso myös

- [Java File-luokka - Javadoc](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [TemporaryFile-luokka - Apache Commons IO](https://commons.apache.org/proper/commons-io/javadocs/api-release/org/apache/commons/io/FileUtils.html#createTempFile(java.lang.String,java.lang.String))