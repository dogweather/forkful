---
title:                "Utvinning av delstrenger"
html_title:           "Kotlin: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utvinning av delstrenger er en prosess der vi tar ut en del av en større tekst eller en streng. Dette gjøres ofte for å få tilgang til en spesifikk del av teksten, slik som et ord eller en setning. Programmere gjør dette for å manipulere data og få mer presis kontroll over arbeidet sitt.

## Slik gjør du:
Kotlin har innebygde funksjoner for å ekstrahere delstrenger, som gjør det enkelt og effektivt å bruke. La oss si du har en streng som heter ```tekst = "Dette er en tekst"```, og du vil bare ha ordet "tekst" fra den. Her er et eksempel på hvordan du kan gjøre dette:
```Kotlin
val start = tekst.indexOfFirst { !it.isLetterOrDigit() } + 1
val end = tekst.indexOfLast { !it.isLetterOrDigit() } + 1
val delstreng = tekst.substring(start, end) // delstreng vil nå være "tekst"
println(delstreng) // Vil skrive ut "tekst"
```

## Dykk dypere:
Denne teknikken blir ofte brukt i tekstbehandling og analyse, og har vært en viktig del av programmering siden tidlig i datamaskinens historie. Det finnes også andre måter å få tilgang til delstrenger på, som for eksempel ved hjelp av regulære uttrykk eller maskering. Det er viktig å huske at indeksering i de fleste programmeringsspråk begynner med 0, så første bokstav i en streng vil ha indeks 0.

## Se også:
- [Kotlins offisielle dokumentasjon om delstrenger](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/substring.html)
- [Tutorial om delstrenger i Kotlin](https://www.programiz.com/kotlin-programming/substring)
- [The Coding Train: Extracting Substrings in Kotlin](https://www.youtube.com/watch?v=1rUPaF6nQZo)