---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til standardfeil (`stderr`) er hvordan programmer rapporterer feil under kjøring. Programmerere bruker det for å skille vanlig utdata fra feilmeldinger for enklere feilsøking og logging.

## Hvordan:
```kotlin
fun main() {
    println("Dette er vanlig utdata.")
    System.err.println("Dette er en feilmelding.")
}

// Forventet utdata:
// Dette er vanlig utdata.
// Dette er en feilmelding.
```
Koden over viser to utskriftslinjer: en til standard utdata (`stdout`), og en til standardfeil (`stderr`). Prøv selv og se forskjellen.

## Dypdykk
Historisk har `stderr` vært brukt for å skille normal utdata fra feilmeldinger. Det har tillatt brukere å omdirigere dem til forskjellige destinasjoner. I Kotlin, som i Java, bruker vi `System.err` for å skrive til `stderr`. Alternativer inkluderer logging-biblioteker som Log4j som tilbyr mer avanserte funksjoner og konfigurasjoner.

I visse tilfeller kan man ønske å omdirigere `stderr` til en fil eller en annen utdatastrøm. Denne handlingen kan utføres både på operativsystemnivå og fra Kotlin-koden ved å bruke `System.setErr()` med en ny `PrintStream`.

## Se Også
- [Kotlin Dokumentasjon](https://kotlinlang.org/docs/reference/)
- [Oracle Java-tutorials – I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [Log4j – Apache Logging Services](https://logging.apache.org/log4j/2.x/)
