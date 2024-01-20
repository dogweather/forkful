---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Java: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sjekke om en katalog eksisterer refererer til å verifisere at det er en angitt sti til en fil i et datasystem. Programmerere gjør dette for å forhindre feil som oppstår når programmet prøver å få tilgang til en ikke-eksisterende katalog.

## Hvordan du gjør det:

I Java har vi flere måter å sjekke om en katalog eksisterer på. Her er noen eksempler:

Bruke File klasse:

```Java
import java.io.File;

public class DirectoryExists {
    public static void main(String[] args) {
        File file = new File("/var/tmp");

        if (file.isDirectory()){
            System.out.println("Directory exists");
        } else {
            System.out.println("Directory does not exist");
        }
    }
}
```

Bruke Java NIO Files klasse:

```Java
import java.nio.file.*;

public class DirectoryExists {
    public static void main(String[] args) {
        Path path = Paths.get("/var/tmp");

        if (Files.exists(path)){
            System.out.println("Directory exists");
        } else {
            System.out.println("Directory does not exist");
        }
    }
}
```

I begge disse eksemplene vil output enten være "Directory exists" eller "Directory does not exist" avhengig av om katalogen eksisterer.

## Dyp Dykk

Sjekke om en katalog eksisterer er en nødvendig teknikk som har blitt implementert i mange programmeringsspråk, og Java er intet unntak. Siden Java 1.0, hvor den første metoden ble introdusert i File-klassen, har Java lagt til alternative metoder som støtter nye funksjoner og økt fleksibilitet gjennom Java NIO.

Alternativene for å gjøre dette har også utvidet, slik som den statiske metoden `Files.exists()`, som var introdusert i Java 7. Den bruker grunnleggende filattributter og er mer fleksibel og effektiv enn den tidligere `File.isDirectory()` metoden.

Implementasjonsdetaljer kan variere basert på brukssaker, for eksempel om du bare vil sjekke tilstedeværelse eller også sjekke at du har riktig tillatelser til å lese/skrive til katalogen.

## Se Også

For mer informasjon om arbeid med filer og kataloger i Java, ta en titt på disse linkene:

- Java Fil API: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html 

- Java NIO Fil API: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html 

- Komplett guide til Java NIO: https://www.baeldung.com/java-nio