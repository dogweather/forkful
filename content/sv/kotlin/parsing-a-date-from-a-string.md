---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:37:06.250144-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att omvandla text till ett datumobjekt. Programmerare gör detta för att kunna hantera datum i beräkningar och logik, oftast när data kommer från användarinmatning eller externt format.

## Hur gör man:
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-12"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateString, formatter)
    
    println(parsedDate) // Sample Output: 2023-04-12
}
```

## Fördjupning
Historiskt sett har datumtolkning varit knepigt med många fällor, speciellt vad gäller tidszoner och lokalisering. Kotlin använder `java.time` paketet (introducerat i Java 8) som är mycket mer robust än de äldre `java.util.Date` och `SimpleDateFormat` klasserna. Möjligheter som `DateTimeFormatter` och `LocalDate` i `java.time` låter oss hantera datum med precision utan att trassla in oss i de vanligaste problemen. Alternativ till standardbiblioteket inkluderar Joda-Time (nu en del av `java.time`) och Apache Commons Lang för mer komplexa datum-manipulationer.

## Se även:
- [DateTimeFormatter Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Tutorial om `java.time` från Baeldung](https://www.baeldung.com/java-8-date-time-intro)
