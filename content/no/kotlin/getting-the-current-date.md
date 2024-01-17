---
title:                "Få dagens dato"
html_title:           "Kotlin: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Å få den nåværende datoen er ganske selvsagt; det er bare å få datoen som er øyeblikket vi er i. Programmere gjør dette for å kunne lage programmer som er avhengige av nåværende dato og tid.

# Hvordan:

Kotlin støtter å få den nåværende datoen gjennom bruk av ```java.util.Date```- og ```java.time.LocalDate```-klassene. Se eksempler nedenfor:

```Kotlin
// Få den nåværende datoen som en java.util.Date-objekt.
val currentDate = Date()

// Få den nåværende datoen som en java.time.LocalDate-objekt.
val currentDate = LocalDate.now()
```

Utskrift av datoer kan gjøres slik:

```Kotlin
// Utskrift av den nåværende datoen som en tekststreng.
println(currentDate.toString())

// Utskrift av bare dato-delen av den nåværende datoen.
println(currentDate.toLocalDate().toString())
```

# Dypdykk

Historisk sett var håndtering av datoer og tid i programmer ofte en krevende og kompleks oppgave. Dette skyldtes blant annet forskjellige kalendere og tidsstandarder som kunne føre til feil i programmer. Med moderne programmeringsspråk som Kotlin, som støtter innebygd funksjonalitet for å håndtere datoer, er denne oppgaven mye enklere.

Alternativer til å få den nåværende datoen inkluderer å bruke tredjeparts biblioteker som Joda-Time eller ThreeTenABP. Disse bibliotekene tilbyr mer omfattende funksjonalitet for å håndtere datoer og tid.

Implementasjonen av å få den nåværende datoen i Kotlin er avhengig av Java-plattformen. Dette betyr at det vil være små forskjeller mellom å få den nåværende datoen i Kotlin og å få den i Java.

# Se også

- Offisiell Kotlin dokumentasjon for å få datoer: https://kotlinlang.org/docs/reference/datetime.html
- ThreeTenABP bibliotek dokumentasjon: https://github.com/JakeWharton/ThreeTenABP
- Joda-Time bibliotek dokumentasjon: https://www.joda.org/joda-time/