---
title:                "Kotlin: Uthenting av understrenger"
simple_title:         "Uthenting av understrenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å ekstrahere substrings kan være en nyttig teknikk når du jobber med tekstbehandlingsoppgaver. Ved å isolere en del av en tekststreng, kan vi enkelt manipulere, sortere eller analysere den på en mer effektiv måte.

## Hvordan

For å ekstrahere en substring i Kotlin, kan du bruke funksjonen `substring()` og angi start- og sluttindeksene for den ønskede substringen. La oss se på et eksempel:

```Kotlin
val tekst = "Hei, jeg heter Kristine"
val substring = tekst.substring(10,16)

println(substring) // utskrift: heter
```

I dette eksempelet bruker vi funksjonen `substring()` for å ekstrahere substringen "heter" fra den opprinnelige teksten. Vi angir startindeksen som er etter kommaet, og sluttindeksen som er før navnet. Dermed får vi ut den ønskede delen av teksten.

## Deep Dive

Det finnes flere måter å ekstrahere substrings på i Kotlin, avhengig av hva du ønsker å oppnå. Du kan for eksempel bruke `substringAfter()` og `substringBefore()` for å isolere en del av teksten basert på et gitt mønster. Eller du kan bruke `subSequence()` som lar deg angi en range av indekser for å ekstrahere en del av teksten.

Det er også viktig å være oppmerksom på at indeksene i en tekststreng starter fra 0, så det kan være lurt å dobbeltsjekke at du angir riktig start- og sluttindeks når du ekstraherer en substring.

## Se Også

* [Kotlins offisielle dokumentasjon for substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
* [Enkel brukerveiledning for Kotlin](https://kotlinlang.org/docs/tutorials/getting-started.html)
* [Grunnleggende tekstbehandling i Kotlin](https://www.geeksforgeeks.org/string-class-in-kotlin/)