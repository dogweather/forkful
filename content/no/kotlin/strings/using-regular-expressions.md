---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:23.495514-07:00
description: "Regul\xE6re uttrykk (regex) er et kraftig verkt\xF8y for tekstbehandling,\
  \ som lar programmerere s\xF8ke, matche og manipulere strenger med avanserte\u2026"
lastmod: '2024-03-13T22:44:40.740102-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) er et kraftig verkt\xF8y for tekstbehandling,\
  \ som lar programmerere s\xF8ke, matche og manipulere strenger med avanserte m\xF8\
  nsters\xF8kingsteknikker."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hva & Hvorfor?

Regulære uttrykk (regex) er et kraftig verktøy for tekstbehandling, som lar programmerere søke, matche og manipulere strenger med avanserte mønstersøkingsteknikker. I Kotlin gjør bruk av regex det mulig å utføre komplekse tekstbehandlingsoppgaver som validering, parsing eller transformasjon på en effektiv måte, noe som gjør det uunnværlig for oppgaver som strekker seg fra enkel strengmanipulering til kompleks tekstanalyse.

## Hvordan:

### Grunnleggende Matching
For å sjekke om en streng matcher et spesifikt mønster i Kotlin, kan du bruke `matches`-metoden til `Regex`-klassen.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Output: true
```

### Finne og Ekstrahere Deler av en Streng
Hvis du vil finne deler av en streng som matcher et mønster, lar Kotlin deg iterere over alle treff:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Dagens dato er 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Output: 07/09/2023
```

### Erstatte Tekst
Å erstatte deler av en tekst som matcher et mønster er enkelt med `replace`-funksjonen:

```kotlin
val input = "Brukernavn: bruker123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Output: Brukernavn: brukerXXX
```

### Splitte Strenger
Del en streng inn i en liste, ved å bruke et regex-mønster som skilletegn:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Output: [1, 2, 3, 4, 5]
```

### Tredjepartsbiblioteker: Kotest
[Kotest](https://github.com/kotest/kotest) er et populært Kotlin-testbibliotek som utvider støtten for innebygd regex i Kotlin, spesielt nyttig for validering i testtilfeller.

```kotlin
// Under forutsetning av at Kotest er lagt til prosjektet ditt
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Dette vil bestå testen hvis input matcher e-postmønsteret.
```

Ved å inkorporere regulære uttrykk i dine Kotlin-applikasjoner, kan du utføre sofistikert tekstbehandling effektivt. Enten du validerer brukerinndata, ekstraherer data eller transformerer strenger, tilbyr regex-mønstre en robust løsning.
