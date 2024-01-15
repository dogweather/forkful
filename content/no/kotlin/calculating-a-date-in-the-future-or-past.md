---
title:                "Beregning av en dato i fremtiden eller fortiden."
html_title:           "Kotlin: Beregning av en dato i fremtiden eller fortiden."
simple_title:         "Beregning av en dato i fremtiden eller fortiden."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er en erfaren Kotlin-utvikler eller bare nysgjerrig på programmering, kan å beregne datoer i fremtiden eller fortiden være en nyttig ferdighet å ha. Enten det er for å planlegge fremtidige hendelser eller spore når en hendelse skjedde, kan å kunne beregne datoer være nyttig i mange situasjoner.

## Hvordan

Beregningslogikk for datoer kan være komplisert, men ved hjelp av Kotlin kan du enkelt håndtere dette. Her er et eksempel på hvordan du kan bruke Kotlin for å beregne datoer i fremtiden og fortiden:

``` Kotlin
// Importer nødvendige biblioteker
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// Definer en funksjon for å beregne datoer
fun beregnDato(dager: Int, måneder: Int, år: Int): LocalDate {
    // Hent dagens dato
    val nå = LocalDate.now()
    // Legg til ønskede dager og måneder
    val fremtidigDato = nå.plusDays(dager.toLong()).plusMonths(måneder.toLong()).plusYears(år.toLong())
    return fremtidigDato
}

// Formater datoen til ønsket format
val formaterDato = DateTimeFormatter.ofPattern("dd.MM.yyyy")
// Kall funksjonen med ønskede parametere og formater datoen for utskrift
println("Fremtidig dato: ${beregnDato(5, 2, 2021).format(formaterDato)}")
```

Dette eksempelet beregner datoen 5 dager, 2 måneder og 2021 år fra nå, og formaterer det som "dd.MM.yyyy" (for eksempel 29.03.2021). Du kan også bytte ut `plusDays`, `plusMonths` og `plusYears` med `minus` for å beregne en dato i fortiden.

## Dypdykk

Som du kan se i eksempelet over, bruker Kotlin `LocalDate` og `DateTimeFormatter` til å håndtere datoer. `LocalDate` er enklere å bruke enn de eldre `Date` og `Calendar` klassene, og `DateTimeFormatter` lar deg formatere datoen på ønsket måte.

I tillegg til å legge til og trekke fra dager, måneder og år, kan du også bruke `with` for å justere en bestemt del av datoen. For eksempel `fremtidigDato.withDayOfMonth(10)` vil justere datoen til den 10. i måneden.

Det er også flere nyttige metoder for å manipulere og håndtere datoer, for eksempel `isBefore`, `isAfter` og `toEpochDay`. Utforsk de ulike metodene og finn ut hva som fungerer best for dine behov.

## Se også

- [Official Kotlin documentation](https://kotlinlang.org/docs/home.html)
- [Java Date and Time API in Kotlin](https://www.baeldung.com/kotlin-dates)
- [Working with Kotlin LocalDate and LocalDateTime](https://betterprogramming.pub/working-with-kotlin-localdate-and-localdatetime-787590f5849a)