---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum innebär att man kontrollerar om ett datum är tidigare, samma eller senare än ett annat datum. Det används ofta av programmerare för att utföra åtgärder baserade på tid, till exempel att kolla när en fil ändrades senast eller planera händelser.

## Hur man gör:

Här är ett exempel på hur man kan jämföra två datum i Kotlin:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 1, 1)
    val date2 = LocalDate.of(2021, 1, 1)

    when {
       date1.isAfter(date2) -> println("date1 är efter date2")
       date1.isBefore(date2) -> println("date1 är före date2")
       else -> println("date1 är samma dag som date2")
    }
}
```

Detta exempel kommer att skriva ut "date1 är före date2" eftersom 1 januari 2020 kommer före 1 januari 2021.

## Djupdykning:

Historiskt sett, innan Java 8 och Kotlin, jämförde programmerare ofta datum med metoder som 'compareTo' eller 'equals'. Med införandet av java.time-paketet i Java 8, vilket också är tillgängligt i Kotlin, har jämförelse av datum blivit betydligt enklare och mer intuitivt.

Ett alternativ till 'isAfter' och 'isBefore' metoder skulle vara att använda 'compareTo' metoden som returnerar en integer. Resultatet är negativt om första datumet är tidigare, positivt om det är senare, och 0 om datumen är lika.

```Kotlin
val comparison: Int = date1.compareTo(date2)
```

Det är viktigt att notera att jämfört med 'compareTo', 'isAfter' och 'isBefore' kanske ger mer tydlig kod.

## Se också:

* Java 8 Date/Time API (java.time) - https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
* Kotlin docs om Comparator - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-comparator/
* Java 'Date' vs 'LocalDate' förklaring - https://stackoverflow.com/questions/37769372/java-time-localdate-vs-java-util-date