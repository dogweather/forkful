---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Kotlin: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Beregne en dato i fremtiden eller fortiden betyr å finne en dato som er et bestemt antall dager, uker, måneder eller år før eller etter en gitt dato. Programmerere gjør dette for å løse vanlige scenarioer som å beregne forfallsdatoer, finne datoer for tilbakevendende hendelser, eller for å måle tidsperioder.

## Hvordan:
I Kotlin bruker vi `plusDays`, `plusWeeks`, `plusMonths` eller `plusYears` metoder for å beregne fremtidige datoer, og deres minus-ekvivalenter for å beregne tidligere datoer. Se eksempelet nedenfor.

```kotlin
import java.time.LocalDate

fun main() {
    val dagensDato = LocalDate.now()
    val tiDagerFram = dagensDato.plusDays(10)
    val tiUkerSidene = dagensDato.minusWeeks(10)

    println("Dagens dato: $dagensDato")
    println("Dato 10 dager fra nå: $tiDagerFram")
    println("Dato 10 uker siden: $tiUkerSidene")
}
```
Hvis du kjører dette, kan eksempelet gi en output som:

```
Dagens dato: 2022-02-23
Dato 10 dager fra nå: 2022-03-05
Dato 10 uker siden: 2022-12-15
```
## Dypdykk
Før JDK 1.8, gjorde vi dette i Java ved hjelp av `Calendar` eller `Date` klassen, som var problematisk på mange måter. Da Kotlin ankom scenen, tok de i bruk de nye dato— og tids— APIene som ble introdusert i JDK 1.8, og dette ble standarden.

Alternativer til de native Kotlin-metodene kan være biblioteker som Joda-Time, men i de fleste tilfeller er Kotlin's innebygde støtte mer enn tilstrekkelig.

Når det gjelder implementering, foregår beregningen av datoer i framtiden eller fortiden på bakgrunn av den gregorianske kalenderen, og tar hensyn til skuddår og varierende lengde på månedene.

## Se også
- [Oracle Guide for Date Time API](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [Joda-Time Bibliotek](https://www.joda.org/joda-time/)