---
title:    "Kotlin: Konvertere en dato til en streng"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en vanlig oppgave i programmering når man jobber med datoer og tidspunkter. Enten det er for å vise en dato på en nettside eller lagre den i en database, er det viktig å kunne konvertere datoen til en format som er enklere å lese og bruke. I denne bloggposten vil jeg vise deg hvordan du enkelt kan gjøre dette ved hjelp av Kotlin.

## Hvordan

Først må vi opprette en `LocalDate`- objekt som inneholder datoen vi ønsker å konvertere. Deretter kan vi bruke `.format()`-funksjonen og spesifisere formatet vi vil ha datoen i. Her er et eksempel:

```Kotlin
val date = LocalDate.of(2021, 10, 20) // Oppretter en LocalDate med datoen 20. oktober 2021

println(date.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))) // Printer ut datoen på formatet dd.MM.yyyy, som vil gi oss "20.10.2021"
```

Vi kan også spesifisere andre formateringsalternativer, for eksempel å legge til tidspunkt og tidsone. Her er et annet eksempel som vil gi oss datoen og tiden på formatet "dd.MM.yyyy HH:mm:ss z":

```Kotlin
val dateTime = LocalDateTime.of(2021, 10, 20, 12, 30, 0) // Oppretter en LocalDateTime med datoen 20. oktober 2021 og klokkeslettet 12:30:00

println(dateTime.format(DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss z"))) // Printer ut datoen og tiden på ønsket format, som vil gi oss "20.10.2021 12:30:00 CEST"
```

Det er viktig å merke seg at noen formateringsalternativer vil kreve tilgang til tids- og klokkeslettstyper, som `LocalDateTime` eller `ZonedDateTime`. Ved å bruke `LocalDate`, vil vi kun få datoen uten tidspunktet.

## Deep Dive

Når du konverterer en dato til en streng, er det viktig å være klar over hvilke formateringsalternativer og tids- og klokkeslettstyper du trenger å bruke, avhengig av hva du ønsker å oppnå. Det er også viktig å sørge for at formatet er leselig for både deg og andre som skal lese og bruke datoen.

En annen ting å være oppmerksom på er at datoen vil bli formatert i henhold til datamaskinens språkinnstillinger. Dette kan bety at datoen vil bli vist i et annet format eller språk enn det du forventer. For å unngå dette kan du spesifisere språk- og lokalinformasjon i formateringsfunksjonen, ved å legge til `.withLocale()`.

## Se også

- [Kotlin docs for Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html)
- [DateTimeFormatter docs](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Tutorialspoint article on formatting dates in Kotlin](https://www.tutorialspoint.com/formatting-dates-in-kotlin-with-localdate-and-datetimeformatter)