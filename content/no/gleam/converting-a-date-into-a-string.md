---
title:                "Gleam: Omgjøring av en dato til en streng"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmerere kan støte på situasjoner hvor de må konvertere en dato til en streng. Dette kan være for å presentere datoer på en mer lesbar måte, eller for å utføre spesifikke beregninger. I Gleam, en ny og spennende funksjonell programmeringsspråk, kan du enkelt utføre denne konverteringen ved hjelp av innebygde funksjoner. I denne bloggposten vil vi gå gjennom hvordan man konverterer en dato til en streng i Gleam.

## Hvordan
Først må du importere biblioteket `gleam/calendar`, som inneholder funksjoner for å håndtere datoer og kalendere. Deretter kan du bruke funksjonen `format_date` for å konvertere en dato til en streng. Her er et eksempel som viser hvordan du kan konvertere en dato til en streng og deretter skrive den ut i konsollen:

```Gleam
import gleam/calendar

let date = Time.utc_now()
let str = Calendar.format_date(date, "%Y-%m-%d")
Log.info(str)
```

Kjører denne koden vil gi følgende resultat:

```
2020-10-20
```

I dette eksempelet brukte vi `%Y-%m-%d` som formateringsmønster, men du kan også bruke andre formateringsalternativer, som beskrevet i dokumentasjonen til `format_date`-funksjonen. Du kan også angi en lokal tidssone ved å legge til en tidszoneparameter i funksjonskallet.

## Dypdykk
Å konvertere en dato til en streng kan virke som en enkel oppgave, men det er faktisk ganske komplekst. Datoer kan representeres på forskjellige måter, for eksempel i ulike kalendere eller tidsformateringssystemer. Derfor er det viktig å velge riktig formateringsmønster for å sikre nøyaktigheten i konverteringen.

I Gleam bruker `Time`-modulen den internasjonale standarden ISO8601 for å representere dato og tid. Dette sikrer at alle datoer blir håndtert på en standardisert og konsistent måte. Når du bruker formateringsmønster i `format_date`-funksjonen, må du sørge for å følge denne standarden for å unngå feil i konverteringen.

## Se Også
- [Dokumentasjon for `gleam/calendar` biblioteket](https://gleam.run/api/master/gleam_calendar.html)
- [Gleam programmeringsspråkets offisielle nettside] (https://gleam.run)