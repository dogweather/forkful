---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:52.484084-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera text\
  \ till ett Datum-objekt. Denna operation \xE4r grundl\xE4ggande f\xF6r applikationer\
  \ som interagerar\u2026"
lastmod: '2024-03-13T22:44:37.880609-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera text\
  \ till ett Datum-objekt."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

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
