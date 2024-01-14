---
title:    "Kotlin: Å få gjeldende dato"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan få dagsaktig dato i Kotlin-programmer? Det er mange grunner til at du kan ønske å gjøre dette, enten det er for å lage datoen for en hendelse eller for å vise datoen i en brukergrensesnitt. Heldigvis er det enkelt å oppnå i Kotlin. 

## Hvordan Gjøre Det

Først og fremst må du importere "java.util.Date" biblioteket i filen der du ønsker å bruke dagsaktuelt dato. Deretter kan du bruke "Date()" konstruktøren for å lage et objekt av denne typen som vil inneholde dagens dato. Dette kan gjøres på følgende måte:

```Kotlin
import java.util.Date

...

val currentDate = Date()
```

Nå har du et objekt av typen "Date" som inneholder dagens dato. Hvis du ønsker å formatere datoen på en bestemt måte, kan du bruke "DateFormat" klassen. Her er et eksempel på hvordan man kan få datoen i formatet "dd/MM/yyyy":

```Kotlin
import java.util.Date
import java.text.DateFormat
import java.text.SimpleDateFormat

...

val currentDate = Date()
val dateFormat = SimpleDateFormat("dd/MM/yyyy")
val formattedDate = dateFormat.format(currentDate)
```

Output fra dette vil være "25/10/2021" (hvis dagens dato er 25. oktober 2021).

## Deep Dive

Dypere informasjon om å få dagsaktuelt dato i Kotlin er å forstå hvordan "Date" og "DateFormat" klassene fungerer. "Date" klassen inneholder metoder for å sette og hente forskjellige datoverdier som år, måned, dag osv. "DateFormat" klassen brukes til å formatere datoen basert på et spesifikt mønster. Det finnes også en "Calendar" klasse som kan være nyttig for å håndtere datoer i Kotlin.

## Se Også

- [Java.util.Date dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java.text.DateFormat dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/text/DateFormat.html)
- [Java.util.Calendar dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)