---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:56:43.820116-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Å sjekke om en mappe eksisterer betyr å bekrefte at en bestemt sti refererer til en mappe på filsystemet før man opererer videre med den. Vi gjør dette for å unngå feil eller å skape nye mapper ved behov.

## How to:
Java tilbyr `Files`-klassen for å sjekke eksistensen av mapper. Her er et enkelt eksempel:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryExistsExample {

    public static void main(String[] args) {
        Path path = Paths.get("/some/directory/path");

        boolean directoryExists = Files.exists(path);

        System.out.println("Directory exists: " + directoryExists);
    }
}
```

Kjører du dette vil utskriften enten være `Directory exists: true` eller `Directory exists: false`, avhengig av om mappen eksisterer eller ikke.

## Deep Dive
Før Java 7, ville en bruke `File`-klassen for å sjekke om en mappe eksisterer. Dette er fortsatt mulig, men `Files`-klassen er mer moderne og gir flere fordeler, som bedre feilhåndtering og støtte for symboliske lenker.

Alternativer:
* `File.exists()` er en enkel, men mindre robust måte.
* `Files.isDirectory()` sjekker at stien er både eksisterende og at det er en mappe.

Implementasjonsdetaljer:
* `Files.exists()` sjekker ikke om stien er en mappe eller en fil. For en mer spesifikk sjekk, bruk `Files.isDirectory()`.
* Operativsystemets tilgangskontroll kan påvirke utfallet av eksistenssjekken.

## See Also
- [Java 7 Files API Documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html)
- [Path Operations (Java Tutorials)](https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html)