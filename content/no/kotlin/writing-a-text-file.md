---
title:    "Kotlin: Skrive en tekstfil"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tekstfiler kan være en viktig del av programmering i Kotlin. Det lar deg lagre og behandle data på en effektiv måte, og kan være nyttig for mange forskjellige oppgaver innen utvikling av applikasjoner.

## Hvordan

Det er enkelt å skrive en tekstfil i Kotlin ved hjelp av innebygde funksjoner og metoder. Først må du åpne en tekstfil ved å opprette et `File` objekt med filbanen som argument. Deretter kan du bruke `FileWriter` klassen til å skrive data til filen.

Her er et eksempel på hvordan du kan skrive en tekstfil med noen linjer av tekst:

```Kotlin
val fil = File("tekstfil.txt") // Oppretter File objekt
val skriver = FileWriter(fil) // Åpner en FileWriter for å skrive til filen
skriver.write("Dette er en tekstfil") // Skriver en linje til filen
skriver.write("med noen ekstra linjer") // Skriver en annen linje
skriver.close() // Lukker FileWriter objektet
```

For å skrive ut data fra tekstfilen kan du bruke `readLines()` metoden, som leser alle linjene fra filen og returnerer dem som en liste:

```Kotlin
val linjer = fil.readLines() // Leser alle linjene fra filen
linjer.forEach { linje -> println(linje) } // Skriver ut hver linje til konsollen
```

Dette vil produsere følgende output:

```
Dette er en tekstfil
med noen ekstra linjer
```

Merk at du må inkludere `FileReader` og `IOException` i import-setningen din for å få dette til å fungere.

## Deep Dive

Når du skriver en tekstfil i Kotlin, kan du også spesifisere hvilken tegnkoding som skal brukes for å lagre dataene. Dette er spesielt viktig hvis du skal jobbe med tekstfiler på forskjellige plattformer, siden standard tegnkoding kan variere.

Du kan spesifisere tegnkoding ved å bruke `charset` argumentet når du oppretter `FileWriter` objektet. Her er et eksempel på å skrive en tekstfil i UTF-8 tegnkoding:

```Kotlin
val skriver = FileWriter(fil, Charset.forName("UTF-8")) // Bruker UTF-8 tegnkoding
```

Du kan også lese tekstfiler med en spesifisert tegnkoding ved å bruke `Charsets` objektet når du kaller `readLines()` metoden:

```Kotlin
val linjer = fil.readLines(Charset.forName("UTF-8")) // Leser filen med UTF-8 tegnkoding
```

For mer informasjon om forskjellige tegnkodinger og hvordan du bruker dem i Kotlin, kan du se offisiell Kotlin dokumentasjon.

## Se Også

- [Offisiell Kotlin Dokumentasjon](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Tegnkoding Dokumentasjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.nio.charset.-charset/)