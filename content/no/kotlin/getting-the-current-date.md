---
title:                "Få gjeldende dato."
html_title:           "Kotlin: Få gjeldende dato."
simple_title:         "Få gjeldende dato."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få nåværende dato kan være nyttig for å vise korrekt informasjon til brukere, beregne forfallsdatoer eller for å loggføre aktiviteter.

## Hvordan gjøre det

For å få nåværende dato i Kotlin, kan du bruke `LocalDate.now()` funksjonen som returnerer en dato objekt som representerer dagens dato.

```Kotlin
val currentDate = LocalDate.now()
println("Dagens dato er: $currentDate")
```

Output: Dagens dato er: 2021-09-14

Hvis du vil få nåværende tidspunkt i tillegg til dato, kan du bruke `LocalDateTime.now()` funksjonen.

```Kotlin
val currentDateTime = LocalDateTime.now()
println("Nå er klokken: $currentDateTime")
```

Output: Nå er klokken: 2021-09-14T14:28:40.123456

Du kan også angi en spesifikk tidsone ved å bruke `now(zone)` funksjonen. For eksempel, hvis du vil ha nåværende dato i New York tidsone, kan du bruke `TimeZone.getTimeZone("America/New_York")` som parameter.

```Kotlin
val currentDateInNY = LocalDate.now(TimeZone.getTimeZone("America/New_York").toZoneId())
println("Dagens dato i New York er: $currentDateInNY")
```

Output: Dagens dato i New York er: 2021-09-14

## Dypdykk

Når du bruker `now()` funksjonen, vil datoen bli hentet fra datamaskinen din sin lokale klokke. Dette betyr at hvis klokken på datamaskinen din ikke er korrekt satt, vil heller ikke datoen som returneres være korrekt.

For å få en mer nøyaktig dato, kan du bruke `Clock` objektet sammen med `now(clock)` funksjonen. `Clock` objektet lar deg spesifisere en bestemt kilde for datoen, for eksempel en NTP-server eller en GPS-klokke.

Et annet alternativ er å bruke `ZonedDateTime` klassen som lar deg få nåværende dato og tid i en bestemt tidsone, samt konvertere mellom ulike tidsone.

## Se også

- Kotlin dokumentasjon for `LocalDate` og `LocalDateTime`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/index.html
- Kotlin dokumentasjon for `ZonedDateTime`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-zoned-date-time/index.html
- Hent nåværende dato og tid i Java: https://www.baeldung.com/java-date-to-localdate-and-localdatetime