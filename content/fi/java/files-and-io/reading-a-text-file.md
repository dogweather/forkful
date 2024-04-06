---
date: 2024-01-20 17:54:21.089500-07:00
description: "N\xE4in se tehd\xE4\xE4n: Esimerkkitulostus."
lastmod: '2024-04-05T21:53:58.028396-06:00'
model: gpt-4-1106-preview
summary: ''
title: Tekstitiedoston lukeminen
weight: 22
---

## Näin se tehdään:
```java
import java.nio.file.*;
import java.io.IOException;

public class TextFileReader {
    public static void main(String[] args) {
        Path filePath = Paths.get("esimerkki.txt");

        try {
            String content = Files.readString(filePath);
            System.out.println("Tiedoston sisältö:");
            System.out.println(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Esimerkkitulostus:
```
Tiedoston sisältö:
Hei, tämä on tekstiesimerkki.
```

## Syväsukellus
Ennen Java 11 -versiota `Files.readString` ei ollut olemassa. Koodarit käyttivät `BufferedReader`- tai `Scanner`-luokkia lukemiseen. Java NIO (New Input/Output) toi paremman suorituskyvyn tiedoston käsittelyyn suuremmilla tiedostoilla ja isommissa järjestelmissä. Vaihtoehtoiset kirjastot, kuten Apache Commons IO, tarjoavat lisäominaisuuksia, mutta Java 11:n jälkeen peruskäyttöön ei yleensä tarvita kolmansien osapuolten kirjastoja.

## Katso myös
- [Oracle Java Documentation - Files.readString](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#readString(java.nio.file.Path))
- [Baeldung - Reading a File into a String](https://www.baeldung.com/java-read-file)
- [Stack Overflow - How do I create a Java string from the contents of a file?](https://stackoverflow.com/questions/4716503/reading-a-plain-text-file-in-java)
