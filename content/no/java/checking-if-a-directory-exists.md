---
title:    "Java: Å sjekke om en mappe eksisterer"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du utvikler programvare, kan det hende du trenger å sjekke om en mappe eksisterer. Dette kan være for å håndtere filer eller for å sikre at programmet ditt fungerer som forventet. Uansett årsak, er det viktig å forstå hvordan man sjekker om en mappe eksisterer i Java.

## Hvordan

For å sjekke om en mappe eksisterer i Java, kan du bruke metoden `exists()` fra `File`-klassen. Her er et eksempel på hvordan dette kan gjøres:

```Java
import java.io.File;

public class CheckDirectory {
  public static void main(String[] args) {
    // Angi stien til mappen du vil sjekke
    File directory = new File("/path/to/directory");
    
    // Sjekk om mappen eksisterer
    if (directory.exists()) {
      System.out.println("Mappen eksisterer.");
    } else {
      System.out.println("Mappen eksisterer ikke.");
    }
  }
}
```

La oss anta at mappen eksisterer, så vil dette gi følgende output:

```sh
Mappen eksisterer.
```

Hvis mappen ikke eksisterer, vil output være:

```sh
Mappen eksisterer ikke.
```

Det er også viktig å merke seg at `exists()`-metoden returnerer en `boolean`-verdi, `true` hvis mappen eksisterer og `false` hvis den ikke gjør det. Dette gjør det enkelt å bruke resultatet i en if-setning eller en annen sammenligning.

## Dypdykk

I tillegg til å bruke `exists()`-metoden, kan du også bruke `isDirectory()`-metoden for å sjekke om det faktisk er en mappe du får returnert. Dette kan være nyttig hvis du jobber med både mapper og filer.

I tillegg kan det være lurt å håndtere eventuelle unntak som kan oppstå ved å bruke `exists()`. For eksempel kan det være at mappen har blitt slettet mens programmet kjører, og da vil det kaste en `SecurityException`.

For å unngå dette kan du omgjøre koden til følgende:

```Java
import java.io.File;
import java.security.SecurityException;

public class CheckDirectory {
  public static void main(String[] args) {
    // Angi stien til mappen du vil sjekke
    File directory = new File("/path/to/directory");
    
    // Sjekk om mappen eksisterer
    try {
      if (directory.exists()) {
        System.out.println("Mappen eksisterer.");
      } else {
        System.out.println("Mappen eksisterer ikke.");
      }
    } catch (SecurityException e) {
      System.out.println("Mappen kan ikke aksesseres.");
    }
  }
}
```

## Se også

- [Java API for File-klassen](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
- [Tutorial om å sjekke om en fil eksisterer i Java](https://www.ntnu.no/wiki/display/itstud/Javakode+-+sjekke+om+fil+eller+mappe+eksisterer)