---
title:    "Kotlin: Å få gjeldende dato"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
En av de mest grunnleggende oppgavene til et programmeringsspråk er å håndtere datoen. Uansett om du utvikler et spill, en mobilapp eller en nettside, er det viktig å kunne få tak i og håndtere den nåværende datoen. Heldigvis gjør Kotlin dette til en enkel oppgave, og i denne bloggposten vil jeg vise deg hvordan du kan få tak i den nåværende datoen ved hjelp av Kotlin.

## Hvordan
Først må vi importere pakken som lar oss få tilgang til dato og tidsfunksjoner i Kotlin. Dette gjøres ved å legge til følgende linje øverst i kildekoden:

```Kotlin
import java.time.*
```

Deretter kan du få tak i den nåværende datoen ved å bruke klassen LocalDate og funksjonen now(). Dette vil gi deg datoen i form av år, måned og dag som vist under:

```Kotlin
val currentDate = LocalDate.now()
```

Utskriften vil da se noe lignende ut: 2021-02-09.

Hvis du ønsker å få tak i mer spesifikk informasjon om datoen, for eksempel bare måned og år, kan du bruke funksjonen get():

```Kotlin
val currentMonth = currentDate.get(ChronoField.MONTH_OF_YEAR)
val currentYear = currentDate.get(ChronoField.YEAR)
```

Utskriften for måned vil da være februar (i form av tall 2), og for år vil det være 2021.

## Deep Dive
I Kotlin kan du også håndtere og manipulere datoer på en fleksibel og enkel måte. Du kan for eksempel få tak i en bestemt fremtidig eller tidligere dato ved hjelp av funksjonen plusDays() for å legge til dager eller minusDays() for å trekke fra dager.

Du kan også formatere datoen ved hjelp av funksjonen format(). Dette lar deg endre hvordan datoen blir presentert, for eksempel å legge til månedsnavn eller endre rekkefølgen på dato, måned og år.

Det er også viktig å merke seg at Kotlin automatisk konverterer datoen til den lokale tiden til datamaskinen. Dette kan være nyttig å vite hvis du jobber med koder som brukes på tvers av ulike tidssoner.

## Se også
- [Kotlin Docs - Dates and Times](https://kotlinlang.org/docs/dates.html)
- [JavaDocs - LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorialspoint - Kotlin Date Time](https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)