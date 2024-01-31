---
title:                "Een tekstbestand lezen"
date:                  2024-01-28T22:05:03.445937-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen betekent dat je programma inhoud uit een bestand opslokt als een string. Programmeurs doen dit om gegevens te verwerken of te analyseren die zich bevinden in bestanden op hun schijf. Het is de basis voor taken zoals configuratie, data-analyse, of zelfs om gewoon je to-do lijst uit te halen.

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
