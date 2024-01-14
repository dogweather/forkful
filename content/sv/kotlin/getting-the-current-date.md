---
title:    "Kotlin: Att hämta aktuellt datum"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Varför

Att kunna hämta den aktuella datumen är en viktig del av många programmeringsprojekt. Genom att kunna få tillgång till den aktuella datumen kan du skapa dynamiska applikationer som kan användas för att registrera tider, schemalägga uppgifter eller bara för att visa användarna när de senast brukade din app. I denna bloggpost kommer jag att dela med mig av hur du kan hämta den aktuella datumen med hjälp av Kotlin.

## Så här gör du

Det finns flera olika sätt att hämta den aktuella datumen i Kotlin. Ett av de enklaste sätten är att använda funktionen `LocalDate.now()`, som returnerar den aktuella datumen som en instans av klassen `LocalDate`.

```Kotlin
val currentDateTime = LocalDate.now()
println(currentDateTime)
```

Det här kommer att skriva ut den aktuella datumen i formatet `YYYY-MM-DD`, till exempel `2021-11-01`. Om du vill ha en annan formatering av datumet kan du använda funktionen `format()` och ange ett anpassat mönster.

```Kotlin
val currentDateTime = LocalDate.now()
val formattedDate = currentDateTime.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(formattedDate)
```

Det här exempel kommer att skriva ut datumet i formatet `DD/MM/YYYY`, till exempel `01/11/2021`. Det finns också andra funktioner som `LocalTime.now()` och `LocalDateTime.now()` som kan användas för att hämta den aktuella tiden eller både datum och tid.

## Djupdykning

Bakom kulisserna använder Kotlin funktioner från Javas `java.time`-paket för att hämta den aktuella datumen. Det betyder att du kan utnyttja alla dess funktioner och metoder för att manipulera datumen på olika sätt. Till exempel kan du använda `plusDays()`-funktionen för att lägga till antal dagar till det aktuella datumet.

```Kotlin
val currentDateTime = LocalDate.now()
val tomorrowsDateTime = currentDateTime.plusDays(1)
println(tomorrowsDateTime)
```

Det här kommer att skriva ut datumet för morgondagen. Genom att kombinera olika funktioner och metoder kan du skapa dynamiska applikationer som utnyttjar den aktuella datumen.

# Se även

- [Java - java.time-paketet](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Standard Bibliotek - java.time-paketet](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/) 
- [Kotlin - `LocalDate`-klassen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)