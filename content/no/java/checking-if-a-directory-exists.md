---
title:    "Java: Sjekke om en mappe eksisterer"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Det er mange situasjoner i en programmeringsverden der vi trenger å sjekke om en mappe eksisterer før vi kan utføre videre oppgaver. Dette er spesielt nyttig når vi jobber med å lagre og hente filer fra en gitt mappe. Ved å sjekke om en mappe eksisterer eller ikke, kan vi unngå potensielle feil i koden vår og sikre at alt fungerer som det skal.

## Hvordan

For å sjekke om en mappe eksisterer i Java, kan vi bruke File-klassen og dens metode `exists()`. Her er et eksempel på koden:

```Java
import java.io.File;

public class DirectoryExists {

    public static void main(String[] args) {

        // Definerer en directory path
        String path = "/brukere/brukernavn/dokumenter/";

        // Oppretter et File-objekt basert på den gitte path
        File directory = new File(path);

        // Sjekker om mappen eksisterer
        if (directory.exists()) {
            System.out.println("Mappen " + path + " eksisterer.");
        } else {
            System.out.println("Mappen " + path + " eksisterer ikke.");
        }
    }
}
```

Output av programmet vil avhenge av om mappen eksisterer eller ikke. Hvis mappen eksisterer, vil output være:

```
Mappen /brukere/brukernavn/dokumenter/ eksisterer.
```

Hvis mappen ikke eksisterer, vil output være:

```
Mappen /brukere/brukernavn/dokumenter/ eksisterer ikke.
```

Det er viktig å merke seg at denne metoden også kan brukes til å sjekke om en fil eksisterer ved å endre pathen til filens plassering.

## Deep Dive

I tillegg til `exists()`-metoden har File-klassen også andre metoder som kan brukes til å arbeide med mapper og filer. For eksempel kan vi bruke `mkdir()` for å opprette en ny mappe og `listFiles()` for å få en liste over alle filer i en gitt mappe.

Det er også viktig å vite at denne metoden bare sjekker om en mappe eksisterer, den sjekker ikke om mappen er tom eller ikke. Hvis du trenger å sjekke om en mappe er tom, kan du bruke `listFiles()`-metoden og sjekke størrelsen på arrayet som returneres. Hvis den er 0, betyr det at mappen er tom.

## Se også

- [File-klassen i Java API](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java IO Tutorial](https://www.tutorialspoint.com/java/java_files_io.htm)