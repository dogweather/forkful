---
title:    "Kotlin: Oppretting av en midlertidig fil"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Skal du lage midlertidige filer i Kotlin? Det korte svaret er at det kan være veldig nyttig når du trenger å lagre data midlertidig mens du utfører en oppgave i programmet ditt.

## Hvordan

For å lage en midlertidig fil i Kotlin kan du bruke "createTempFile()" -funksjonen. Her er et eksempel på hvordan du kan bruke denne funksjonen i koden din:

```Kotlin
// Oppretter en midlertidig fil med navnet "temp_file.txt" i mappen du kjører programmet fra
val tempFile = createTempFile("temp_file", ".txt")

// Skriver data til den midlertidige filen
tempFile.writeText("Dette er en midlertidig fil")

// Henter data fra den midlertidige filen og skriver den ut
println(tempFile.readText())

// Resultat:
// Dette er en midlertidig fil
```

I dette eksemplet brukte vi "writeText()" -funksjonen for å skrive data til den midlertidige filen, og "readText()" -funksjonen for å få tilgang til dataene senere.

Når du er ferdig med å bruke den midlertidige filen, kan du slette den ved å bruke "delete()" -funksjonen:

```Kotlin
// Sletter den midlertidige filen
tempFile.delete()
```

## Deep Dive

Når vi bruker "createTempFile()" -funksjonen i Kotlin, er det noen viktige ting å merke seg. Først av alt, blir den midlertidige filen automatisk opprettet i systemets midlertidige mappe. Dette kan være forskjellig fra system til system, men det pleier å være et sted som er avsatt til midlertidige filer.

En annen viktig ting å huske på er at den midlertidige filen blir automatisk slettet når programmet ditt avsluttes. Dette er flott hvis du vil sikre at ingen midlertidige filer blir liggende igjen etter at programmet er ferdig, men det kan også være et problem hvis du trenger å beholde filen for en lengre tid. I så fall kan du bruke "use" -funksjonen for å spesifisere en kodeblokk der filen vil bli slettet når blokken er ferdig:

```Kotlin
// Oppretter en midlertidig fil i en spesifikk mappe og beholder den til slutten av kodeblokken
createTempFile("temp_file", ".txt", yourCustomFolder).use { tempFile -> 
    // Gjør noe med filen her
}
```

## Se også

- [Kotlin Offisiell Dokumentasjon om midlertidige filer](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Offisiell Kotlin Blogg - "Using temporary files in Kotlin"](https://blog.jetbrains.com/kotlin/2020/09/using-temporary-files-in-kotlin/)