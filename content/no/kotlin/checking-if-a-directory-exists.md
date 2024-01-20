---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:57:16.974551-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe finnes betyr ganske enkelt å bekrefte at den er der før du utfører operasjoner som å lese eller skrive til den. Programmerere gjør dette for å unngå feil og kræsj når applikasjonen forventer at bestemte mapper skal være på plass.


## Hvordan:
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val directoryPath = Paths.get("/noen/vei/til/mappen")
    
    if (Files.exists(directoryPath)) {
        println("Mappen finnes!")
    } else {
        println("Mappen finnes ikke.")
    }
}
```
Kjør dette, og du får "Mappen finnes!" eller "Mappen finnes ikke." avhengig av om mappen er der eller ikke.


## Dypdykk:
Før Java 7 og Kotlin, sjekket man ofte om mapper eksisterte ved å bruke `File`-klassen og dens `exists()`-metode, noe som ikke alltid var like pålitelig da det ikke skilte mellom filer og mapper. Med `java.nio.file.Files` og `Paths` i Java 7 (som Kotlin arver), fikk utviklere mer robuste verktøy for filhåndtering. Alternativer inkluderer bruk av Kotlin's `File` klasse direkte:

```kotlin
import java.io.File

fun main() {
    val directory = File("/noen/vei/til/mappen")

    if (directory.exists() && directory.isDirectory) {
        println("Mappen finnes og er en faktisk mappe!")
    } else {
        println("Mappen finnes ikke eller er ikke en mappe.")
    }
}
```

Ved å legge til `isDirectory`, sørger vi for at det vi finner ikke bare eksisterer, men også at det er en mappe.

Noen taler om å lytte etter filesystem-events med `WatchService` hvis man trenger å reagere i sanntid til endringer i filsystemet. 


## Se Også:
- Java NIO File API: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Kotlin's File Class: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Introduksjon til Java NIO WatchService: https://docs.oracle.com/javase/tutorial/essential/io/notification.html