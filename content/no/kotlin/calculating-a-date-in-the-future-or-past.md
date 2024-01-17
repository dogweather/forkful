---
title:                "Beregning av en dato i fremtiden eller tidligere"
html_title:           "Kotlin: Beregning av en dato i fremtiden eller tidligere"
simple_title:         "Beregning av en dato i fremtiden eller tidligere"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Hvis du noensinne har jobbet med en app eller et program som involverer datoer, har du kanskje støtt på behovet for å beregne en dato i fremtiden eller fortiden. Dette kan være nyttig for å planlegge fremtidige hendelser eller utforske historiske datoer. Programmere bruker dette konseptet for å enkelt håndtere datoer og tidsstempel i sine applikasjoner.

## Hvordan:
Hvis du vil beregne en dato i fremtiden eller fortiden, kan du bruke Kotlin Date-klassen. Her er et eksempel på hvordan du gjør det:

```Kotlin
// Lager en ny Date-variabel som representerer nåværende dato og tid
var nåtid = Date()

// Bruker Date-klassen sin add-metode for å legge til 5 dager 
var fremtidigDato = nåtid.add(Calendar.DAY_OF_MONTH, 5)

// Printer ut den fremtidige datoen
println(fremtidigDato)
```

Output: Tir Jul 06 12:32:30 CEST 2021

## Deep Dive:
Beregning av datoer i fremtiden eller fortiden har vært en viktig funksjon i programmering i lang tid. Før utviklingen av avanserte datostyringssystemer, var det vanlig å bruke algoritmer for å håndtere datoer. I dag er beregning av datoer en del av standardbiblioteket til de fleste programmeringsspråk, inkludert Kotlin.

Det finnes også andre metoder for å beregne datoer, for eksempel gjennom bruk av tidssone-konvertering og bruk av ulike biblioteker og verktøy. Det er viktig å velge riktig metode avhengig av behovene til ditt program eller prosjekt.

## Se også:
- Offisiell Kotlin dokumentasjon: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/
- Kotlin Date-klassen på GitHub: https://github.com/JetBrains/kotlin/tree/master/libraries/stdlib/jvm/src/java/util/Date.kt