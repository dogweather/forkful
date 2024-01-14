---
title:                "Kotlin: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Lurer du på om en mappe eksisterer i ditt Kotlin-program? Det kan være viktig å vite for å sikre at dine filbehandlingsfunksjoner fungerer riktig. I denne bloggposten skal vi se nærmere på hvordan du kan sjekke om en mappe eksisterer i Kotlin-programmering.

## Hvordan
Først må vi importere nødvendige pakker:

```Kotlin
import java.io.File
import java.io.IOException
```

Deretter kan vi bruke metoden `exists()` fra `File`-klassen til å sjekke om en mappe er tilstede. Her er et eksempel:

```Kotlin
fun sjekkMappe(mappenavn: String) {
    val mappe = File(mappenavn)
    if(mappe.exists()){
        println("Mappen $mappenavn eksisterer.")
    }else{
        println("Mappen $mappenavn eksisterer ikke.")
    }
}

sjekkMappe("dokumenter")
```

Dette vil resultere i følgende utskrift dersom mappen `dokumenter` eksisterer:

```
Mappen dokumenter eksisterer.
```

Eller dersom mappen ikke eksisterer:

```
Mappen dokumenter eksisterer ikke.
```

## Deep Dive
Det finnes flere ulike metoder for å sjekke om en mappe eksisterer i Kotlin. I tillegg til `exists()` kan vi også bruke `isDirectory()` som returnerer en boolsk verdi basert på om mappen er en gyldig mappe eller ikke.

```Kotlin
if(mappe.isDirectory()){
    // gjør noe
}
```

Det er viktig å håndtere mulige feil som kan oppstå når man sjekker for eksisterende mapper. For eksempel kan det være at det ikke er tilgang til en mappe eller at den ikke finnes i det hele tatt. Derfor bør man alltid wrappe sjekken i en `try-catch`-blokk.

```Kotlin
try {
    // sjekk for eksisterende mapper her
} catch(e: IOException){
    println("En feil oppstod under sjekken: ${e.message}")
}
```

## Se også
- [Java - How to check if a directory exists](https://www.baeldung.com/java-check-directory-exists)
- [Kotlin - File Handling](https://www.geeksforgeeks.org/kotlin-file-handling/)
- [Kotlin - Exception Handling](https://www.tutorialspoint.com/kotlin/kotlin_exception_handling.htm)