---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Java: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av Java-programmering. Det lar deg forsikre deg om at koden din fungerer som den skal, og at du kan håndtere situasjoner der mappen ikke finnes.

## Hvordan

For å sjekke om en mappe eksisterer, kan du bruke metoden `exists()` fra Java's `File` klasse. Først må du importere `File` klassen:

```Java
import java.io.File;
```

Deretter kan du opprette en `File` objekt og bruke `exists()` metoden på den. Her er et eksempel som sjekker om mappen "test" eksisterer i nåværende arbeidsmappe:

```Java
File mappe = new File("test");
if (mappe.exists()) {
    System.out.println("Mappen eksisterer!");
} else {
    System.out.println("Mappen eksisterer ikke.");
}
```

Dette vil skrive ut enten "Mappen eksisterer!" eller "Mappen eksisterer ikke.", avhengig av om mappen eksisterer eller ikke. Du kan også sjekke om en mappe eksisterer på en spesifikk plassering ved å gi den fullstendige banen i `File` konstruktøren.

## Dypdykk

Når du bruker `exists()` metoden, må du være oppmerksom på at den ikke bare sjekker om en mappe med det gitte navnet eksisterer. Den kan også returnere `true` hvis en fil med det samme navnet finnes. Dette er fordi `File` klassen representerer både filer og mapper. 

En annen viktig ting å merke seg er at `exists()` metoden bare sjekker om mappen eksisterer når metoden kalles. Hvis mappen ble opprettet eller slettet etter at metoden ble kalt, vil den ikke reflektere den nye statusen. Derfor er det alltid best å sjekke om mappen eksisterer rett før du skal bruke den.

## Se også

- Dokumentasjon for Java's `File` klasse: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Tutorial om Java filbehandling: https://www.codejava.net/java-se/file-io/4-ways-to-copy-file-in-java
- Stack Overflow diskusjon om å sjekke om en mappe eksisterer: https://stackoverflow.com/questions/1816673/how-do-i-check-if-a-file-exists-in-java