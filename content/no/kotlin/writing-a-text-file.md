---
title:                "Kotlin: Skriver en tekstfil"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange gode grunner til å skrive en tekstfil i Kotlin. Kanskje ønsker du å lagre data på en måte som er enkel å lese og endre for deg selv eller andre programmere. Eller kanskje du bare vil øve deg på å bruke Kotlin til å håndtere filer. Uansett årsak, å kunne skrive til en tekstfil kan være en nyttig ferdighet å ha.

## Hvordan

Skriving til en tekstfil i Kotlin kan gjøres ved å bruke et `FileWriter` objekt og en `write()` metode. Først må du opprette et `File` objekt som representerer tekstfilen du vil skrive til. Deretter kan du initialisere `FileWriter` objektet og bruke `write()` metoden til å skrive til filen. Her er et eksempel:

```Kotlin
val file = File("navn_pa_fil.txt") // opprett et File objekt for å representere filen
val fileWriter = FileWriter(file) // initialiser FileWriter objektet
fileWriter.write("Dette er en tekst som blir skrevet til filen.") // skriv til filen
fileWriter.close() // lukk FileWriter objektet
```

Når du kjører dette eksemplet, vil teksten bli skrevet til en fil med navn `navn_pa_fil.txt` som blir lagret i samme mappe som Kotlin-koden din.

## Dypdykk

Nå som du har lært å skrive en enkel tekstfil i Kotlin, kan du også utforske flere muligheter. For eksempel kan du bruke `BufferedWriter` og `println()` metoden til å skrive til filen, noe som kan gjøre koden din mer lesbar. Du kan også bruke `FileReader` og `BufferedReader` til å lese fra filer på en lignende måte.

## Se også

For mer informasjon om å håndtere filer i Kotlin, kan du sjekke ut disse ressursene:

- [Offisiell Kotlin dokumentasjon for filbehandling](https://kotlinlang.org/docs/reference/basic-types.html#classes-and-inheritance)
- [Kotlin Filbehandler Tutorial](https://www.tutorialkart.com/kotlin/kotlin-read-write-plain-text-file/)
- [Kotlin for Android: Filbehandling](https://www.raywenderlich.com/686603-android-file-management-with-kotlin)