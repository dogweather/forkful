---
title:                "Kotlin: Hente nåværende dato"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kunne hente inn og håndtere datoer er en viktig del av enhver programmeringsoppgave. Uansett om du lager en kalenderapplikasjon, holder oversikt over bestillinger eller bare ønsker å vise datoen på skjermen, så er det å kunne hente inn dagens dato en nødvendighet i mange tilfeller.

## Hvordan

Med Kotlin er det enkelt å få tak i dagens dato ved hjelp av innebygde funksjoner. Først må vi importere klassen "LocalDate" fra biblioteket "java.time". Deretter kan vi bruke funksjonen "now()" for å hente ut dagens dato og lagre den i en variabel. Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
import java.time.LocalDate

val today: LocalDate = LocalDate.now()

print(today)
```

Dette vil gi oss følgende utskrift:

```Kotlin
2021-10-19
```

Vi kan også hente ut andre detaljer fra datoen, som for eksempel måneden, dagen og året. Dette gjøres ved hjelp av funksjonene "monthValue", "dayOfMonth" og "year". Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
import java.time.LocalDate

val today: LocalDate = LocalDate.now()

val month: Int = today.monthValue
val day: Int = today.dayOfMonth
val year: Int = today.year

print("$year-$month-$day")
```

Dette vil gi oss følgende utskrift:

```Kotlin
2021-10-19
```

## Dypdykk

Bak kulissene bruker Kotlin den moderne datatypen "LocalDate" for å representere datoer. Denne typen er en del av biblioteket "java.time", som ble introdusert i Java 8. "LocalDate" gjør det enkelt å håndtere datoer og tilbyr blant annet funksjoner for konvertering mellom forskjellige datoformater og utregning av fremtidige eller tidligere datoer.

En annen viktig funksjon er "of()", som gjør det mulig å opprette en spesifikk dato ved å angi år, måned og dag som parametere. Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
import java.time.LocalDate

val date: LocalDate = LocalDate.of(2021, 10, 31)

print(date)
```

Dette vil gi oss følgende utskrift:

```Kotlin
2021-10-31
```

## Se også

- Kotlin sin offisielle dokumentasjon om "java.time": https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/
- Java sin offisielle dokumentasjon om "java.time": https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html