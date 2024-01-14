---
title:                "Kotlin: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fortiden eller fremtiden kan være nyttig for å planlegge hendelser eller håndtere tidsrelatert data. Dette kan være spesielt nyttig for programmerere som jobber med applikasjoner som håndterer kalender og tidsstyring.

## Hvordan

For å beregne en dato i Kotlin, kan vi bruke funksjonen `plus` eller `minus` på `LocalDate`-objekter. Følgende kode viser hvordan vi kan beregne en dato ti dager frem i tid:

```Kotlin
val today = LocalDate.now()
val futureDate = today.plusDays(10)
println(futureDate) // Output: 2021-10-22
```

På samme måte kan vi bruke `minus` for å beregne en dato i fortiden. For eksempel, la oss beregne en dato ti år tilbake i tid:

```Kotlin
val today = LocalDate.now()
val pastDate = today.minusYears(10)
println(pastDate) // Output: 2011-10-12
```

Det er også mulig å beregne en dato basert på en annen tidsenhet, for eksempel måneder eller år. Dette kan gjøres ved å bruke funksjonene `plusMonths` og `plusYears`, eller `minusMonths` og `minusYears` for beregning i fortiden.

## Dypdykk

Når man beregner en dato ved hjelp av `plus` eller `minus` funksjonene, tar Kotlin automatisk hensyn til skuddår og riktige månedslengder. Dette gjør det enklere for oss å beregne riktig dato uten å måtte håndtere disse detaljene selv.

Det er også mulig å endre datoen for et `LocalDate`-objekt ved å bruke funksjonen `withDayOfMonth`, som lar oss endre dagen i måneden, og `withMonth`, som lar oss endre måneden. Dette kan være nyttig for å justere en dato til en spesifikk dag eller måned.

## Se også

- [Dokumentasjon for LocalDate funksjoner i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/index.html)
- [Tutorial: Manipulere datoer i Kotlin](https://www.baeldung.com/kotlin/date-time-manipulation)