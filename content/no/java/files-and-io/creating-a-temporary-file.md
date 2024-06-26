---
date: 2024-01-20 17:41:02.550722-07:00
description: "Hvordan: Oppretting av midlertidige filer i Java er ikke nytt. Det har\
  \ v\xE6rt en del av Java siden de f\xF8rste versjonene, men m\xE5ten det h\xE5ndteres\
  \ p\xE5 har\u2026"
lastmod: '2024-04-05T22:50:54.697506-06:00'
model: gpt-4-1106-preview
summary: Oppretting av midlertidige filer i Java er ikke nytt.
title: Opprette en midlertidig fil
weight: 21
---

## Hvordan:
```Java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Opprett midlertidig fil
            Path tempFile = Files.createTempFile("minMidlertidigFil", ".txt");
            
            // Skriv ut filstien til den midlertidige filen
            System.out.println("Midlertidig fil opprettet på: " + tempFile.toString());

            // Slett filen ved programslutt
            tempFile.toFile().deleteOnExit();
        } catch (IOException e) {
            System.out.println("En feil oppstod under oppretting av midlertidig fil: " + e);
        }
    }
}
```

Sample output:
```
Midlertidig fil opprettet på: C:\...\minMidlertidigFil1234567890.txt
```

## Dypdykk:
Oppretting av midlertidige filer i Java er ikke nytt. Det har vært en del av Java siden de første versjonene, men måten det håndteres på har forbedret seg. I tidligere Java-utgaver, brukte man `File`-klassen for å opprette og håndtere midlertidige filer. Fra og med Java 7, introduserte NIO-pakken (`java.nio.file`) et mer robust API som `Files`-klassen, gir bedre feilhåndtering og mer lesbar kode.

Et alternativ til å bruke `Files.createTempFile` er å lage en midlertidig fil manuelt ved hjelp av `File.createTempFile`, men dette er mindre foretrukket i moderne Java-kode på grunn av fordelene nevnt over.

Når man jobber med midlertidige filer, er det viktig å sørge for at de slettes etter bruk for å unngå å fylle opp disken og potensielt skape sikkerhetsrisikoer. Metoden `deleteOnExit()` er praktisk, men det er viktig å være klar over at den bare kjører når virtuell maskin avsluttes. Hvis applikasjonen kjører i lang tid, eller filen skaper sensitiv informasjon, bør den slettes så fort den ikke trengs lenger.

## Se Også:
- [Files.createTempFile Javadoc](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#createTempFile(java.lang.String,java.lang.String,java.nio.file.attribute.FileAttribute...))
