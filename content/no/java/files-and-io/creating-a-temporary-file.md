---
aliases:
- /no/java/creating-a-temporary-file/
date: 2024-01-20 17:41:02.550722-07:00
description: "\xC5 lage en midlertidig fil i Java betyr \xE5 skape en fille som er\
  \ tenkt \xE5 v\xE6re kortvarig, ofte for \xE5 h\xE5ndtere data midlertidig under\
  \ applikasjonens kj\xF8retid.\u2026"
lastmod: 2024-02-18 23:08:53.789129
model: gpt-4-1106-preview
summary: "\xC5 lage en midlertidig fil i Java betyr \xE5 skape en fille som er tenkt\
  \ \xE5 v\xE6re kortvarig, ofte for \xE5 h\xE5ndtere data midlertidig under applikasjonens\
  \ kj\xF8retid.\u2026"
title: Opprette en midlertidig fil
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lage en midlertidig fil i Java betyr å skape en fille som er tenkt å være kortvarig, ofte for å håndtere data midlertidig under applikasjonens kjøretid. Programmerere gjør dette for sikker datalagring, for å håndtere store data som ikke får plass i minnet, eller for å unngå å påvirke programmets opprinnelige datakilde.

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
