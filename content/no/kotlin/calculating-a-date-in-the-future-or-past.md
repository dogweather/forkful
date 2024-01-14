---
title:    "Kotlin: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Å beregne datoer i fremtiden eller fortiden er en viktig del av programmering. Enten det er for å opprette en kalenderfunksjon, planlegge fremtidige hendelser eller analysere historiske data, vil å kunne beregne datoer være nyttig for mange programmer.

# Hvordan

For å beregne en dato i fremtiden eller fortiden i Kotlin, kan du bruke en kombinasjon av Java's `LocalDate` og Kotlin's `plusDays` eller `minusDays` funksjoner. Her er et eksempel på å beregne datoen 10 dager frem i tid:

```kotlin
val today = LocalDate.now()
val futureDate = today.plusDays(10)
println("Datoen 10 dager frem i tid er: $futureDate")
```

Dette vil resultere i følgende utskrift:

`Datoen 10 dager frem i tid er: 2020-05-23`

På samme måte kan du beregne en dato i fortiden ved hjelp av `minusDays` funksjonen. Her er et eksempel på å beregne datoen 5 dager tilbake i tid:

```kotlin
val today = LocalDate.now()
val pastDate = today.minusDays(5)
println("Datoen 5 dager tilbake i tid er: $pastDate")
```

Dette vil resultere i følgende utskrift:

`Datoen 5 dager tilbake i tid er: 2020-05-08`

Det er også mulig å bruke andre tidsenheter som måneder og år ved hjelp av `plusMonths` og `plusYears` funksjoner.

# Dypdykk

Det er viktig å være oppmerksom på forskjellen mellom `plusDays` og `plus` funksjoner i Kotlin. Mens `plusDays` tar inn en `Int` verdi og legger til det angitte antallet dager, tar `plus` funksjonen inn et `TemporalAmount` objekt og kan dermed legge til år, måneder, dager og andre tidsenheter.

Det er også viktig å være klar over at datoen som returneres fra disse funksjonene er en ny `LocalDate` objekt, og at den opprinnelige datoen forblir uendret. Du må lagre resultatet i en ny variabel eller tilordne det til den opprinnelige datoen for å endre den.

# Se også

- [Kotlin dokumentasjon for LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- [Java dokumentasjon for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Kotlin dokumentasjon for plusDays funksjonen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-temporal/plus-days.html)
- [Kotlin dokumentasjon for plus funksjonen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-temporal/plus.html)