---
title:    "Kotlin: Att hämta aktuellt datum"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den nuvarande datum i kod är en viktig del av många programmeringsprojekt. Det kan hjälpa till att hålla informationen i en applikation aktuell och hjälpa till att spåra aktiviteter och händelser över tid. Att lära sig att få den nuvarande datum i Kotlin är en viktig färdighet för alla som vill utveckla användbara program.

## Hur man gör

Det första steget för att få den nuvarande datum i Kotlin är att importera klassen `java.util.Calendar`. Sedan kan du använda den för att hämta datum, tid och tidszon. Här är ett exempel på hur du kan skriva en funktion för att hämta dagens datum i en sträng:

```Kotlin
import java.util.Calendar
fun getCurrentDate(): String {
    val calendar = Calendar.getInstance()
    val day = calendar.get(Calendar.DAY_OF_MONTH)
    val month = calendar.get(Calendar.MONTH)
    val year = calendar.get(Calendar.YEAR)
    return "$day/$month/$year"
}
```

För att få den nuvarande datumet i en viss tidszon, måste du först skapa en instans av klassen `java.util.TimeZone` med den tidszon du vill ha. Sedan använder du den instansen vid skapandet av en instans av `java.util.Calendar`. Här är ett exempel på hur man hämtar den nuvarande datumet i en specifik tidszon:

```Kotlin
import java.util.Calendar
import java.util.TimeZone
fun getCurrentDate(timezone: TimeZone): String {
    val calendar = Calendar.getInstance(timezone)
    val day = calendar.get(Calendar.DAY_OF_MONTH)
    val month = calendar.get(Calendar.MONTH)
    val year = calendar.get(Calendar.YEAR)
    return "$day/$month/$year"
}
```

Exempel på output från dessa funktioner:

```10/5/2021``` för den första funktionen, och ```10/5/2021``` i GMT-tidszonen för den andra funktionen.

## Djupdykning

Det finns flera andra metoder och egenskaper som kan användas för att hämta och manipulera datumet i Kotlin. Några av de vanligaste inkluderar `calendar.add()`, `calendar.set()`, och `calendar.get()`. Genom att använda dessa metoder kan du till exempel lägga till eller dra bort en viss tid från ett datum eller sätta ett visst datum. Det kan vara användbart för att skapa en kalenderapplikation eller för att beräkna förloppet av en viss händelse.

## Se också

* [Kotlin Date and Time APIs](https://kotlinlang.org/docs/tutorials/kotlin-for-py/date-time.html)
* [Java Calendar Class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
* [Kotlin Standard Library - Datetime Package](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-datetime/index.html)