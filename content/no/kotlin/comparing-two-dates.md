---
title:                "Kotlin: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Hvorfor
Sammenligning av to datoer er en vanlig oppgave innen programmering, spesielt når man arbeider med applikasjoner som krever håndtering av tid og dato. Ved å forstå hvordan man sammenligner datoer i Kotlin, kan man enkelt implementere funksjonene som trengs for å sikre nøyaktig og pålitelig håndtering av datoer i sin kode.

##Slik gjør du det
For å sammenligne to datoer i Kotlin, kan man bruke funksjonen `compareTo()`, som returnerer et heltall som representerer resultatet av sammenligningen. Hvis dato1 er større enn dato2, vil funksjonen returnere et tall større enn 0. Hvis dato1 er mindre enn dato2, vil funksjonen returnere et tall mindre enn 0. Hvis de to datoene er like, vil funksjonen returnere 0.

```Kotlin
val date1 = LocalDate.of(2021, 10, 15)
val date2 = LocalDate.of(2021, 12, 25)

println(date1.compareTo(date2)) // -71 

val date3 = LocalDate.of(2021, 5, 1)
val date4 = LocalDate.of(2021, 5, 1)

println(date3.compareTo(date4)) // 0 

val date5 = LocalDate.of(2021, 8, 1)
val date6 = LocalDate.of(2021, 3, 20)

println(date5.compareTo(date6)) // 133 
```

I eksemplene over sammenlignes forskjellige datoer ved hjelp av `compareTo()`-funksjonen. Merk at funksjonen også kan brukes på andre datatyper som `LocalDateTime` og `LocalTime`.

##Dypdykk
Under the hood bruker `compareTo()`-funksjonen den såkalte "natural ordering" av datoer. Dette betyr at datoer blir sammenlignet basert på deres verdier for år, måned og dag. Bruk av `compareTo()`-funksjonen kan også bidra til å unngå feil og unøyaktigheter når man håndterer datoer, da dette er en pålitelig og standardisert måte å sammenligne datoer på.

##Se også
- [Java Date/Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Standard Library](https://kotlinlang.org/docs/stdlib/)