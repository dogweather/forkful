---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:03.445937-07:00
description: 'Hoe: Een bestand lezen is een fluitje van een cent in Java, vooral met
  `java.nio.file`. Hier is een snel voorbeeld.'
lastmod: '2024-03-13T22:44:50.699943-06:00'
model: gpt-4-0125-preview
summary: Een bestand lezen is een fluitje van een cent in Java, vooral met `java.nio.file`.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
Een bestand lezen is een fluitje van een cent in Java, vooral met `java.nio.file`. Hier is een snel voorbeeld:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.util.stream.Stream;

public class FileReadExample {
    public static void main(String[] args) {
        Path filePath = Path.of("example.txt");

        try (Stream<String> lines = Files.lines(filePath)) {
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Als je dit uitvoert met `example.txt` dat "Hallo, bestandlezers!" bevat, zou de uitvoer zijn:

```
Hallo, bestandlezers!
```

## Diepgaand
Java is geëvolueerd. Vroeger moest je zelf streams en readers beheren - veel boilerplate. Het `java.io` pakket was helemaal in, met `FileReader` en `BufferedReader` vaak gezien in het wild. Toen kwam `java.nio`, met kanalen en buffers voor meer controle.

Nu is `java.nio.file` nog hoger niveau. `Files` en `Paths` vereenvoudigen de klus. Het bovenstaande voorbeeld gebruikt `Files.lines`, dat lijnen lui streamed, geweldig voor grote bestanden. Je krijgt ook try-with-resources, dat automatisch streams sluit om lekken te voorkomen.

Alternatieven? `Scanner` is handig voor het parsen. Apache Commons IO en Google's Guava hebben hulpmiddelen voor complexere taken, als je ze nodig hebt. Toch brengt standaard Java je meestal al een heel eind.

Wat betreft de implementatie, bestandscodering doet ertoe. `Files.lines` gaat standaard uit van UTF-8, maar je kunt een andere specificeren. Aan de andere kant moet je bij `BufferedReader` de `Charset` vooraf instellen als het niet de standaard is.

## Zie ook
Voor meer pit, kijk eens naar deze:

- De [`Files`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html) klasse in Java's officiële documentatie.
- [Reading, Writing, and Creating Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html) voor een grondige doorloop.
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/) voor een robuuste bibliotheek van bestands-IO-hulpmiddelen.
