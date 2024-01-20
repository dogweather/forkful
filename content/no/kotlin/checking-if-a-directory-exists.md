---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Kotlin: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe eksisterer betyr å bestemme om en spesifikk mappe allerede er på plass på et gitt datafil-system. Programmers gjør dette for å unngå feil som kan oppstå når du prøver å opprette eller får tilgang til en mappe som ikke eksisterer.

## Hvordan til:
I Kotlin kan vi enkelt sjekke om en mappe eksisterer ved å bruke `exists()` funksjonen. Funksjonen returnerer "true" hvis mappen eksisterer, og "false" hvis den ikke gjør det.

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main(args: Array<String>) {
    val path = Paths.get("/sti/til/mappen")
    val eksisterer = Files.exists(path)

    if (eksisterer) {
        println("Mappen eksisterer")
    } else {
        println("Mappen eksisterer ikke")
    }
}
```
Hvis mappen er til stede vil dette skriptet skrive ut "Mappen eksisterer". Hvis mappen ikke er til stede vil det skrive ut "Mappen eksisterer ikke".

## Dyp Dykk
Historisk sett, før introduksjonen av NIO API i Java 7 og i koden deretter Kotlin, ble filsystemoperasjoner utført ved hjelp av `File` klassen, som også har en `exits()` funksjon. Alternativt kan du også bruke `File().isDirectory` funksjonen i Kotlin, men denne vil returnere "true" selv om den angitte stien er en fil og ikke en mappe. Så det er enda bedre å bruke `Paths.get()` og `Files.exists()` funksjonene som vi gjorde i eksemplet vårt.

Den underliggende implementeringen av `exists()` går gjennom katalogindeksen på filsystemet, noe som gjør det mye raskere enn å manuelt skanne gjennom hele filsystemet.

## Se Også
2. Java Dokumentasjon på Path API: [Link](https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html)
3. Stack Overflow Diskusjoner: [Link](https://stackoverflow.com/questions/46561458/how-to-check-if-folder-exists-if-not-the-folders-should-be-created-in-kotlin)