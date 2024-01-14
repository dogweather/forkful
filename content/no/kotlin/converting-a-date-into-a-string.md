---
title:    "Kotlin: Konvertering av en dato til en streng"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være en vanlig utfordring når man jobber med Kotlin programmering. Det er viktig å kunne gjøre dette riktig for å vise datoer riktig til brukere eller lagre dem i en database. Derfor er det viktig å forstå hvordan å gjøre dette på riktig måte.

## Hvordan

Det er flere måter å konvertere en dato til en streng på i Kotlin, avhengig av hva slags format man ønsker å ha på datoen. Her er et eksempel på hvordan man kan konvertere en dato til en streng og få utputt i et spesifikt format:

```Kotlin
// Opprett en dato-variabel
val dato = Date()
// Definer ønsket format
val format = SimpleDateFormat("dd-MM-yyyy")
// Konverter datoen til en streng
val datoSomStreng = format.format(dato)
// Skriv ut datoen
println(datoSomStreng)
// Output: 28-01-2021
```

Legg merke til at i dette eksempelet brukte vi klassen `SimpleDateFormat` for å definere ønsket datoformat. Det er viktig å spesifisere riktig format i henhold til hva man ønsker, ellers kan det resultere i feil datoformat.

En annen måte å konvertere en dato til en streng på er ved hjelp av Kotlin `LocalDate` og `DateTimeFormatter`:

```Kotlin
// Opprett en dato-variabel
val dato = LocalDate.now()
// Definer ønsket format
val format = DateTimeFormatter.ofPattern("dd-MMM-yyyy")
// Konverter datoen til en streng
val datoSomStreng = dato.format(format)
// Skriv ut datoen
println(datoSomStreng)
// Output: 28-Jan-2021
```

I dette eksempelet bruker vi `DateTimeFormatter` til å spesifisere ønsket datoformat og `LocalDate` for å få dagens dato.

## Dypdykk

Når man jobber med datokonvertering i Kotlin, er det viktig å forstå at datoen alltid blir representert som et nummer i bunn og grunn. Derfor må man være nøye med å spesifisere riktig format når man skal konvertere datoen til en streng. Hvis man ikke gjør dette, kan det føre til feil datoformat og dermed forvirring for brukerne.

Man kan også bruke `SimpleDateFormat` og `DateTimeFormatter` for å konvertere datoen til et annet språk enn standard engelsk, ved å bruke `Locale` klassen. Dette kan være nyttig hvis man ønsker å vise datoer i et annet språk for internasjonale brukere.

## Se også

- [Kotlin Docs - Datokonverteringer](https://kotlinlang.org/docs/datetime.html#date-and-time-conversions)
- [Java SimpleDateFormat dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Java DateTimeFormatter dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)