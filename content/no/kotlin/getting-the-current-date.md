---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dagens dato betyr å få den nøyaktige datoen i nåtid som år, måned og dag. Innføringen av datoer er kritisk i programmering for å holde styr på hendelser og lagre data nøyaktig.

## Hvordan:
Her er en enkel fremgangsmåte å hente dagens dato på Kotlin:

```Kotlin
import java.time.LocalDate
fun main() {
   val dagensDato = LocalDate.now()
   print("Dagens dato er: $dagensDato")
}
```

Du burde se noe slikt:

```
Dagens dato er: 2022-10-15
```

## Dypdykk
Historisk sett, innen programmering og spesielt innen Java, har vi brukt `Date` fra `java.util` pakken for å hente og manipulere datoer. Senere kom `Calendar` som en forbedret versjon, men begge har noen mangler når det kommer til brukervennlighet og effektivitet. 

I Kotlin har vi muligheten til å bruke `LocalDate` fra `java.time` pakken. Dette er en del av JSR-310 spesifikasjonen introdusert i Java 8 for å løse problemene som oppstod med `Date` og `Calendar`.

Alternativt er det flere tredjepartsbiblioteker som Joda-Time og ThreeTenABP som kan brukes til dato- og tidsoperasjoner.

Effektiv implementering av datoer krever forståelsen av tidszoner. 

## Se Også
For en mer inngående forståelse av dato og tid i Kotlin, sjekk ut disse linkene:

1. [Kotlin dokumentasjon](https://kotlinlang.org/docs/dates-and-times.html)
2. [Java 8 date and time guide](https://www.baeldung.com/java-8-date-time-intro)
3. [Joda-Time Library](https://www.joda.org/joda-time/)
4. [ThreeTenABP Library](https://github.com/JakeWharton/ThreeTenABP)