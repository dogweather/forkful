---
title:    "Kotlin: Sammenligner to datoer"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Mange programmerere står ofte overfor en utfordring når de trenger å sammenligne to datoer. Dette kan være for å sjekke om en hendelse har skjedd etter en annen eller for å beregne tidsforskjellen mellom to hendelser. Uansett årsaken, er det viktig å vite hvordan man effektivt kan sammenligne to datoer i Kotlin.

# Hvordan gjør man det

Sammenligning av datoer i Kotlin kan gjøres på flere måter, men den mest effektive måten er ved hjelp av `LocalDate` og `ChronoUnit` klassene. Følgende er et eksempel på hvordan man kan sammenligne to datoer og finne tidsforskjellen mellom dem:
```Kotlin
val dato1 = LocalDate.of(2021, Month.AUGUST, 15)
val dato2 = LocalDate.now()

// Sjekker om dato2 er etter dato1
if (dato2.isAfter(dato1)) {
    println("Dato 2 kommer etter dato 1")
}

// Beregner tidsforskjellen i dager mellom dato2 og dato1
val dager = ChronoUnit.DAYS.between(dato1, dato2)
println("Det er $dager dager mellom dato1 og dato2")
```

I dette eksempelet bruker vi `LocalDate` og `ChronoUnit` for å sammenligne datoer og finne tidsforskjellen mellom dem. Dette er en enkel og effektiv måte å sammenligne datoer på i Kotlin.

# Dypdykk

Hvis du ønsker en mer avansert måte å sammenligne datoer på, kan du også bruke `ZonedDateTime` klassen. Denne klassen lar deg sammenligne datoer i forskjellige tidssoner og utføre mer komplekse manipulasjoner. Her er et eksempel på hvordan du kan bruke `ZonedDateTime` for å finne tidsforskjellen mellom to datoer:
```Kotlin
val dato1 = ZonedDateTime.of(2021, Month.DECEMBER.value, 1, 0, 0, 0, 0, ZoneId.of("Europe/Oslo"))
val dato2 = ZonedDateTime.of(2022, Month.JANUARY.value, 1, 0, 0, 0, 0, ZoneId.of("Asia/Tokyo"))

// Beregner tidsforskjellen i timer mellom dato2 og dato1
val timer = ChronoUnit.HOURS.between(dato1, dato2)
println("Det er $timer timer mellom dato1 og dato2")
```

Som du kan se, å bruke `ZonedDateTime` gir flere muligheter og fleksibilitet når man sammenligner datoer.

# Se også

- [Kotlin Dokumentasjon](https://kotlinlang.org/docs/home.html)
- [Kotlin Reference for Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-date-time/index.html)