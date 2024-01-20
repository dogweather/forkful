---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er en operasjon der vi henter inn innholdet i en fil til vår applikasjon. Dette er vanlig når vi skal behandle data, innstillinger eller dokumenter i programmeringsoppgaver. 

## Slik Gjør Du:

Her er et enkelt eksempel på hvordan du kan lese innhold fra en tekstfil i Kotlin.

```Kotlin
import java.io.File

fun main() {
    val innhold = File("test.txt").readText()
    println(innhold)
}
```
Når du kjører koden, vil den skrive ut innholdet i 'test.txt'. Hvis for eksempel 'test.txt' har teksten "Hei, Verden!", vil utskriften være:

```Kotlin
Hei, Verden!
```

## Dypdykk

Leseoperasjoner har vært grunnleggende for programmering siden starten, da mye av programvareutvikling handler om å manipulere lagret data. I Kotlin, har vi ulike metoder for å lese en tekstfil, slik som bruk av BufferedReader, FileInputStream eller Scanner, avhengig av det spesifikke behovet.

Alternativt kan du bruke `readLines()`- metoden for å lese filen linje for linje. Denne metoden kan være praktisk når filen er stor eller når du trenger å behandle data linje for linje.

Det er viktig å merke seg at stien til tekstfilen er relativ til plasseringen av programmet. Hvis tekstfilen ikke finnes, vil `readText()` kaste en FileNotFoundException.

## Se Også

For mer informasjon om filbehandling i Kotlin, sjekk ut:

- [Lese og skrive til filer](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html) i den offisielle Kotlin-dokumentasjonen.
- [Håndtering av unntak](https://kotlinlang.org/docs/exceptions.html) for når filer ikke eksisterer eller andre problemer oppstår.