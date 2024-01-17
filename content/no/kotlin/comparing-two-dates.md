---
title:                "Sammenligning av to datoer"
html_title:           "Kotlin: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer er en vanlig oppgave for programmerere. Dette innebærer å sammenligne to datoer og bestemme hvilken som er tidligere enn den andre. Dette er nødvendig for å håndtere datoer i programmering og for å gjøre ulike beregninger med dem.

## Slik gjør du det:
```Kotlin
// Lager to datoer
val dato1 = LocalDate.of(2021, 10, 28)
val dato2 = LocalDate.of(2021, 11, 2)

// Bruker compareTo() funksjonen for å sammenligne datoer
println(dato1.compareTo(dato2)) // Utskrift: -5 (dato1 er tidligere enn dato2)

// Bruker isBefore() funksjonen for å sjekke om dato1 er tidligere enn dato2
println(dato1.isBefore(dato2)) // Utskrift: true
```

## Dypdykk:
Sammenligning av datoer har vært et problem for programmerere i lang tid. Dette skyldes utfordringene knyttet til å håndtere ulike kalendere og tidsformater. Alternativet til å bruke innebygde funksjoner som compareTo() og isBefore() er å konvertere datoene til et numerisk format og sammenligne dem. Dette kan være mer komplekst og kreve mer kode.

## Se også:
- [Kotlin Dokumentasjon om Datoer](https://kotlinlang.org/docs/datetime.html)
- [Java 8 Dato og Tid API](https://www.baeldung.com/java-8-date-time-intro)
- [Wikipedia Side om Datoer i Programmering](https://en.wikipedia.org/wiki/Date_calculations_(programming))