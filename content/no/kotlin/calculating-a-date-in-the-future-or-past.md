---
title:                "Kotlin: Kalkulere en dato i fremtiden eller fortiden"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge og organisere aktiviteter eller for å sjekke viktige datoer. Det kan også være morsomt å se hva datoen var på en spesiell hendelse i fortiden eller når en spesiell dato vil falle i fremtiden.

## Hvordan
Det finnes forskjellige måter å beregne en dato i fremtiden eller fortiden på i Kotlin. En enkel metode er å bruke Date og Calendar klassene.

```Kotlin
import java.util.*

val dato = Calendar.getInstance() 
dato.add(Calendar.DAY_OF_YEAR, 10) // Legger til 10 dager på nåværende dato
println("Datoen om 10 dager er ${dato.time}")
```
Outputen av denne koden vil være 10 dager frem i tid fra kjøretidspunktet. Denne metoden kan også brukes til å beregne datoer i fortiden ved å bruke `add()` funksjonen med et negativt tall.

En annen måte å beregne en dato i fremtiden eller fortiden på er å bruke `LocalDate` og `LocalDateTime` klassene som er en del av Java 8 dato og tid biblioteket.

```Kotlin
import java.time.LocalDate
import java.time.LocalDateTime

val dato = LocalDate.now()
val fremtidigDato = dato.plusDays(30) // Legger til 30 dager på nåværende dato
val fortidigDato = dato.minusYears(2) // Trekker fra 2 år fra nåværende dato

val dateTime = LocalDateTime.now()
val fremtidigDateTime = dateTime.plusMonths(6) // Legger til 6 måneder på nåværende dato og tid

println("Datoen om 30 dager er $fremtidigDato")
println("Datoen for 2 år siden var $fortidigDato")
println("Dato og tid om 6 måneder er $fremtidigDateTime")
```
Denne metoden gir mer fleksibilitet og nøyaktighet når du beregner datoer og tidspunkter.

## Dykk dypere
Hvis du vil beregne mer avanserte datoer, som for eksempel faktiske dager i fremtiden eller fortiden basert på en bestemt hendelse, kan du også bruke `TemporalAdjuster` klassen som er en del av Java 8 dato og tid biblioteket.

```Kotlin
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.temporal.TemporalAdjusters

val dato = LocalDate.of(2020, 7, 15) // Spesifikk dato for å beregne fra
val nesteMandag = dato.with(TemporalAdjusters.next(DayOfWeek.MONDAY)) // Henter neste mandag fra denne datoen
val sisteDagIAret = dato.with(TemporalAdjusters.lastDayOfYear()) // Henter siste dag i året

println("Neste mandag fra 15. juli 2020 er $nesteMandag")
println("Siste dag i året 2020 er $sisteDagIAret")
```
Denne metoden gjør det mulig å beregne datoer basert på ulike kriterier, som ukedag eller posisjon i måneden.

## Se også
- [Java 8 dato og tid dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin offisiell dokumentasjon](https://kotlinlang.org/docs/reference/)