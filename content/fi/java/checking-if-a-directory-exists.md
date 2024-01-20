---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Java: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tarkistaminen, onko hakemisto olemassa, on ohjelmiston kehittäjän tapa varmistaa, että hakemisto on järjestelmässä ennen sen käyttöä. Tämä on tärkeää, koska se estää virhetilanteita, kuten tiedoston luku- tai kirjoitusyrityksiä olemattomaan hakemistoon.

## Kuinka tehdään:

Voit tarkistaa, onko hakemisto olemassa, Java'ssa `Files` luokan `exists()` metodia käyttäen:

```Java
import java.nio.file.*;

public class DirectoryExists {
    public static void main(String[] args) {
        Path dirPath = Paths.get("/path/to/directory");
        
        if (Files.exists(dirPath)) {
            System.out.println("Hakemisto on olemassa.");
        } else {
            System.out.println("Hakemisto ei ole olemassa.");
        }
    }
}
```

Tämän ohjelman ajaminen tulostaa "Hakemisto on olemassa" jos hakemisto on olemassa ja "Hakemisto ei ole olemassa" jos se ei ole.

## Syväsukellus:

Tarkistaaksesi, onko hakemisto olemassa, Java ohjelmoijat käyttivät usein `File` luokan `exists()` metodia ennen Java 7:n julkaisua. Uudemmissa versioissa suositellaan `Files` luokan `exists()` metodin käyttämistä, koska se on monipuolisempi ja tarjoaa parempaa virheenkäsittelyä.

Toinen vaihtoehto on `Files` luokan `notExists()` metodi, joka tarkistaa, onko hakemisto olemassa. Siinä on se ero, että se palauttaa `true`, jos hakemisto ei ole olemassa.

Hakemiston olemassaolon tarkistamisen yksityiskohtana on, että vaikka `exists()` metodi palauttaa `true`, ei välttämättä tarkoita, että ohjelma pystyy kirjoittamaan tai lukemaan hakemistosta. Tämän voi tarkistaa erikseen `Files.isWritable()` tai `Files.isReadable()` metodeilla.

## Katso myös:

[Lue lisää `Files` luokan koko API:sta Java dokumentaatiosta täältä.](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)

[Opas Java 7:n tiedosto I/O (NIO.2) toiminnallisuudesta.](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)

[Lisätietoa Java ohjelmoinnista Oracle'ssa.](https://www.oracle.com/java/technologies/javase/documentation/api-overview.html)