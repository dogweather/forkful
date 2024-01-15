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

## Hvorfor

Å sammenligne to datoer i et program er ofte nødvendig for å håndtere tidsrelaterte data. Dette kan være nyttig for å finne ut av når en hendelse fant sted, eller sammenligne ulike tider for å ta beslutninger i koden.

## Slik gjør du det

For å sammenligne to datoer i Kotlin kan du bruke `compareTo()` funksjonen. Her er et eksempel på hvordan du kan sammenligne to datoer:

```Kotlin
// Opprett to datoer
val dato1 = LocalDate.of(2021, 10, 4) 
val dato2 = LocalDate.of(2021, 10, 6)

// Sammenlign datoene
val resultat = dato1.compareTo(dato2)

// Resultatet vil være en verdi som representerer forholdet mellom datoene
// Hvis dato1 er før dato2 vil resultatet være negativt
// Hvis dato1 er etter dato2 vil resultatet være positivt
// Hvis dato1 og dato2 er like vil resultatet være 0
```

Du kan også bruke `equals()` funksjonen for å sjekke om to datoer er like:

```Kotlin
// Opprett to datoer
val dato1 = LocalDate.of(2021, 10, 4) 
val dato2 = LocalDate.of(2021, 10, 4)

// Sjekk om datoene er like
val erLike = dato1.equals(dato2)

// Resultatet vil være en boolsk verdi som representerer om datoene er like eller ikke
```

## Graving dypt

Når du sammenligner to datoer i Kotlin, bruker `compareTo()` funksjonen faktisk `Comparable` grensesnittet. Dette grensesnittet lar deg sammenligne objekter av samme type ved å implementere `compareTo()` funksjonen. Dette betyr at du også kan sammenligne dine egne datoklasser ved å gjøre dem `Comparable` og definere hvordan sammenligningen skal utføres.

## Se også

- [Kotlin Offisiell Dokumentasjon](https://kotlinlang.org/docs/home.html)
- [Sammenligne to datoer i Java](https://www.baeldung.com/java-compare-two-dates) 
- [Sammenligning av datoer i JavaScript](https://www.w3schools.com/js/js_date_methods.asp)