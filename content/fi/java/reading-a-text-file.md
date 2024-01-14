---
title:                "Java: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi tekstitiedostojen lukeminen on tärkeää?

Tekstiedostojen lukeminen on välttämätöntä monissa ohjelmointiprojekteissa, sillä ne tarjoavat helpon tavan tallentaa ja lukea tietoja. Ne ovat myös erittäin hyödyllisiä tekstin käsittelyyn liittyvissä tehtävissä, kuten tietojen muokkaamisessa ja analysoinnissa.

## Miten lukea tekstitiedosto Javassa?

Javan File-luokka tarjoaa helpon tavan lukea ja kirjoittaa tekstitiedostoja. Seuraavassa esimerkissä näytämme, miten voit lukea tiedoston sisällön ja tulostaa sen näytölle:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class TextFileReader {
    public static void main(String[] args) {
        // Luodaan uusi File-objekti ja annetaan sille tiedoston nimi
        File file = new File("tekstitiedosto.txt");
        
        try {
            // Luodaan uusi Scanner-objekti tiedoston lukemiseen
            Scanner scanner = new Scanner(file);

            // Luetaan tiedoston sisältö ja tulostetaan se näytölle
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                System.out.println(line);
            }

            // Suljetaan Scanner-objekti
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Tiedostoa ei löytynyt.");
        }
    }
}
```

Tulostus:
```
Tervetuloa lukemaan tekstitiedostoa!
Tässä on esimerkki tiedostosta.
Voit muokata ja lisätä tähän haluamiasi tekstejä.
```

## Syvempää tietoa tekstitiedostojen lukemisesta

Tekstiedostojen lukeminen Javassa perustuu lukijan (esim. Scanner) käyttöön, joka lukee tiedostosta merkki kerrallaan. Javan FileReader-luokka lukee tiedoston tavu kerrallaan, joka tarjoaa hieman tehokkaamman tavan lukea suurikokoisia tiedostoja. Voit myös käyttää Javan Path-luokkaa pääsemään käsiksi tiedostopolkuun ja tiedoston nimeen.

## Katso myös

- Java File-luokka: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Java Scanner-luokka: https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html
- Javan tiedostonhallinta: https://docs.oracle.com/javase/tutorial/essential/io/file.html