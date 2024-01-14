---
title:    "Kotlin: Beregning av datoer i fremtiden eller fortiden"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Hvorfor

Det kan være mange grunner til å måtte beregne en dato i fremtiden eller fortiden. Det kan være for å planlegge en reise, fødselsdag, eller for å sjekke når en kontrakt utløper. Hvordan man beregner disse datoene kan variere avhengig av behov, men heldigvis har Kotlin en rekke verktøy som gjør dette enkelt.

# Hvordan

```kotlin
// Legg til antall dager, måneder eller år til en given dato
val startDato = LocalDate.now()
val nyDato = startDato.plusDays(7) // 7 dager senere

// Trekk fra antall dager, måneder eller år fra en gitt dato
val forrigeDato = startDato.minusMonths(3) // 3 måneder senere

// Spesifiser en dato ved å angi år, måned og dag
val spesifikkDato = LocalDate.of(2022, Month.APRIL, 18) // 18. april 2022

// Sjekk om en dato er før eller etter en annen dato
val erFremtidig = startDato.isAfter(nyDato) // false
val erFortidig = startDato.isBefore(forrigeDato) // true
```

## Deep Dive

Beregning av datoer innebærer mer enn bare en enkel tillegg eller trekk fra antall dager. Det er viktig å ta hensyn til faktorer som skuddår, tidssoner og lokale nasjonale helligdager. Heldigvis har Kotlin et utvidet bibliotek for Java Time API som gjør dette enklere.

For å ta hensyn til skuddår, kan man bruke metoden `withDayOfYear()` som spesifikt beregner datoer basert på antall dager i året. Når det gjelder lokale helligdager, kan man bruke klassen `HolidayCalendars` som gir programmerere tilgang til standard kalendere fra forskjellige land og regioner.

## Se også

- [Offisiell Kotlin dokumentasjon for Java Time API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-date-time/index.html)
- [How to: Manipulate Time and Dates in Kotlin](https://www.baeldung.com/kotlin/dates-time-api)
- [Java Time API tutorial på YouTube (engelsk)](https://www.youtube.com/watch?v=KEVQdUdOqzY)