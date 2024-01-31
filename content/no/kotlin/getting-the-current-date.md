---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:15:27.201950-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å hente dagens dato betyr å få den nåværende datoen fra systemet. Programmerere gjør dette for å tidsmerke hendelser, håndtere tidsavhengige funksjoner, eller bare vise datoer til brukerne.

## How to: (Slik gjør du:)
Kotlin gjør det lett å få tak i dagens dato. Her er en enkel måte å gjøre det på:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dagens dato er: $today")
}
```

Dersom du kjører koden, vil det ligne på:

```
Dagens dato er: 2023-04-05
```

## Deep Dive (Dypdykk)
Før Kotlin og moderne Java-versioner brukte vi `java.util.Date`, men den var beryktet for mange problemer, inkludert mutable state og dårlig design. Med introduksjonen av Java 8 kom `java.time`-pakken, også kjent som JSR-310, som Kotlin også bruker, og forandret spillet. 

Et alternativ til `LocalDate` er `Calendar`-klassen, men den er mindre intuitiv og mer komplisert å bruke. `LocalDate` gir deg bare datoen, mens `Calendar` gir både dato og tid, noe som ikke alltid er nødvendig.

Implementasjonsdetaljer i `LocalDate` tar hensyn til tidssone og internasjonalisering, slik at du får den riktige datoen uansett hvor koden kjøres. Dette er kritisk for applikasjoner som opererer over flere tidssoner.

## See Also (Se Også)
For mer informasjon og avanserte bruksmåter, besøk:

- Oracle Java-dokumentasjon: [Date Time API](https://docs.oracle.com/javase/tutorial/datetime/)
- GitHub Kotlin-prosjektside: [Kotlin GitHub](https://github.com/JetBrains/kotlin)
