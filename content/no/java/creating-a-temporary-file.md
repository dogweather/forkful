---
title:    "Java: Oppretting av midlertidig fil"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å opprette en midlertidig fil kan være en viktig del av Java-programmering, spesielt når det kommer til håndtering av store datamengder. Å opprette midlertidige filer kan hjelpe til med å organisere og sikre at data blir håndtert på en effektiv måte.

## Hvordan
Det finnes flere måter å opprette midlertidige filer i Java på, men en av de enkleste er å bruke File.createTempFile() metoden. Denne metoden tar inn to argumenter, et prefiks og en suffiks, for å lage en unik filnavnsekvens. Se eksempelet nedenfor for å få en bedre forståelse av hvordan dette fungerer:

```Java
import java.io.File;
import java.io.IOException;

public class TempFileExample {

    public static void main(String[] args) {
        try {
            // Opprett en midlertidig fil med prefikset "temp" og suffikset ".txt"
            File tempFile = File.createTempFile("temp", ".txt");
            
            // Skriv ut filnavnet
            System.out.println("Midlertidig fil opprettet: " + tempFile.getName());
            
            // Slett filen
            tempFile.delete();
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Eksempel-utdata:
```
Midlertidig fil opprettet: temp4049596361000421714.txt
```

## Dypdykk
I tillegg til å lage en unik filnavnsekvens, har File.createTempFile() metoden også muligheten til å spesifisere en filsti og en File-permisjon. Dette kan være nyttig hvis du for eksempel ønsker å lage en midlertidig fil i et spesifikt mappe. Her er et eksempel på hvordan dette kan gjøres:

```Java
// Definerer en mappe der den midlertidige filen skal opprettes
File tempDir = new File("C:/temp/");

// Opprett en midlertidig fil med prefikset "temp" og suffikset ".txt" i mappen du har valgt
File tempFile = File.createTempFile("temp", ".txt", tempDir);
```

## Se også
- [Java - File API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Create Temporary Files in Java](https://www.baeldung.com/java-temporary-files)