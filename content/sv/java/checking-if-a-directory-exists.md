---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:57:10.764261-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog existerar betyder att du verifierar om en specifik mapstruktur finns på din dator eller server. Programutvecklare gör detta för att undvika fel (som fil-eller mappsaknadsfel) när de skriver, läser eller uppdaterar filer.

## Hur man gör:
```java
import java.nio.file.*;

public class DirectoryExists {
    public static void main(String[] args) {
        // Sätt katalogens sökväg här
        Path directoryPath = Paths.get("/din/katalog/här");

        // Kontrollera om katalogen existerar
        boolean directoryExists = Files.exists(directoryPath);

        // Skriv ut resultatet
        System.out.println("Katalogen existerar: " + directoryExists);
    }
}

/*
Exempel på utskrift om katalogen existerar:
Katalogen existerar: true

Exempel på utskrift om katalogen inte existerar:
Katalogen existerar: false
*/
```

## Djupdykning:
Historiskt sett använde Java-kodare klassen `File` för att kontrollera filers och katalogers existens. Från och med Java 7 introducerades NIO (New Input/Output), som inkluderar `Files` och `Path` klasserna för en mer modern och flexibel filhantering. Alternativa sätt att kontrollera en katalogs existens är `Files.isDirectory(directoryPath)` eller genom att fånga `NoSuchFileException` när du försöker öppna en fil i den katalogen. Att använda `Files.exists()` är dock enklare när du bara vill kolla om katalogen finns.

## Se också:
- Java Documentation for `Files` class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html
- Java Documentation for `Path` class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Path.html
- Tutorial on Java NIO File IO: https://www.baeldung.com/java-nio2-file-api