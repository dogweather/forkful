---
title:                "Kotlin: Sammenligning av to datoer"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave i programmering, spesielt i Kotlin. Å behandle datoer kan være utfordrende og feil kan fort oppstå hvis man ikke bruker riktige metoder. I denne bloggposten vil vi se på hvorfor det er viktig å kunne sammenligne to datoer, og hvordan man kan gjøre det på en effektiv måte ved hjelp av Kotlin.

## Slik gjør du det

For å sammenligne to datoer i Kotlin, kan vi bruke metoden `isAfter()` og `isBefore()` fra `LocalDate`-klassen. Vi starter først med å definere to datoer, og deretter bruke disse metodene for å sammenligne dem, som vist i eksempelet nedenfor:

```Kotlin
val date1 = LocalDate.of(2021, 9, 1)
val date2 = LocalDate.of(2021, 8, 1)

println(date1.isAfter(date2))
println(date1.isBefore(date2))
```

Output:

```
true
false
```

Her ser vi at `isAfter()` metoden returnerer `true` fordi `date1` kommer etter `date2`, mens `isBefore()` metoden returnerer `false` fordi `date1` er før `date2`.

En annen viktig metode å bruke er `isEqual()` som sjekker om to datoer er like. Dette er spesielt nyttig når man sammenligner datoer som inneholder tidsinformasjon. Se eksempelet nedenfor:

```Kotlin
val dateTime1 = LocalDateTime.of(2021, 10, 1, 10, 30, 0)
val dateTime2 = LocalDateTime.of(2021, 10, 1, 12, 0, 0)

println(dateTime1.isEqual(dateTime2))
```

Output:

```
false
```

Selv om datoene er på samme dato, vil `isEqual()` returnere `false` fordi de har forskjellige klokkeslett.

## Dypdykk

Når vi sammenligner datoer i Kotlin, er det viktig å være klar over eventuelle forskjeller i formatering og tidssoner. Dette kan påvirke resultatet av sammenligningen og føre til unøyaktigheter. Det er derfor viktig å alltid definere datoer med samme formatering og sørge for at de er i samme tidsone før man sammenligner dem.

En annen ting å være oppmerksom på er at `isAfter()` og `isBefore()` metodene ikke tar hensyn til tidszoneinformasjon, kun dato og klokkeslett. Hvis man ønsker å sammenligne datoer basert på tidszone, kan man bruke metoden `zonedDateAndTime.isAfter()` og `zonedDateTime.isBefore()`, hvor `zonedDateTime` er en `ZonedDateTime`-objekt.

## Se også

- [LocalDate Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [LocalDateTime Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date-time/)
- [ZonedDateTime Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-zoned-date-time/)