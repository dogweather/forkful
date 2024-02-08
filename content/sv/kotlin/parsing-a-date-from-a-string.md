---
title:                "Analysera ett datum från en sträng"
date:                  2024-02-03T19:14:52.484084-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera ett datum från en sträng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att konvertera text till ett Datum-objekt. Denna operation är grundläggande för applikationer som interagerar med datum som användare matar in eller som hämtas från externa datasamlingar, vilket möjliggör enkel manipulering och formatering enligt behov.

## Hur man gör:
Kotlin stödjer datumtolkning genom `java.time`-paketet, som introducerades i Java 8. Här är en enkel metod med hjälp av `LocalDateTime` och ett specifikt mönster:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val datum = parseDateFromString(dateString)
    println(datum)  // Utdata: 2023-04-01T12:00
}
```

För mer flexibilitet, eller för att hantera datum från externa källor som API:er, kan du använda ett externt bibliotek som Joda-Time (även om det är mindre vanligt nu när `java.time` är robust). Dock är det föredraget att hålla sig till den moderna metoden som tillhandahålls av JDK för de flesta Kotlin-applikationer.

För att tolka ett datum i Kotlin utan att använda externa bibliotek, kan du också använda klassen `SimpleDateFormat` för versioner före Java 8 eller Android-API-nivåer som saknar stöd för `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    ettDatum = parseDateUsingSimpleDateFormat(dateString)
    println(ettDatum)  // Utdata kommer att variera beroende på din tidszon, t.ex., Lör Apr 01 12:00:00 GMT 2023
}
```

Kom ihåg att alltid ställa in tidszonen om du arbetar med `SimpleDateFormat` för att undvika oväntade avvikelser i tolkade datum.
