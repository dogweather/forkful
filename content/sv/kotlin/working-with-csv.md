---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV, eller Comma-Separated Values, är ett enkelt filformat för att lagra tabelldata. Programmerare använder det för att enkelt utbyta data mellan olika system och för att bearbeta stora mängder information med minimala filstorlekar.

## How to:
Att hantera CSV-filer i Kotlin kan ske enkelt med hjälp av bibliotek som `kotlin-csv`. Först, lägg till beroendet i `build.gradle`:
```kotlin
dependencies {
    implementation("com.github.doyaaaaaken:kotlin-csv-jvm:0.15.2")
}
```

Läs en CSV-fil:
```kotlin
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader

val rows = csvReader().readAll(File("data.csv"))
rows.forEach { row ->
    println(row)
}
```

Skriva till en CSV-fil:
```kotlin
import com.github.doyaaaaaken.kotlincsv.dsl.csvWriter

val data = listOf(
    listOf("id", "namn", "ålder"),
    listOf("1", "Eva", "34"),
    listOf("2", "Oskar", "42")
)

csvWriter().writeAll(data, File("output.csv"))
```

## Deep Dive
Historiskt sett härstammar CSV från tidiga datordagar, ibland använd som mellanhand mellan olika applikationer. Alternativ till CSV inkluderar JSON och XML, som båda kan hantera mer komplex data men till högre kostnad vad gäller filstorlek och komplexitet. Kotlin-bibliotek som `kotlin-csv` förenklar CSV-hantering genom att erbjuda enkel läsning och skrivning av filer utan att behöva hantera filströmmar direkt.

## See Also
- Kotlin CSV library dokumentation: [kotlin-csv](https://github.com/doyaaaaaken/kotlin-csv)
- Officiell Kotlin-dokumentation: [Kotlin Programming Language](https://kotlinlang.org/docs/home.html)
- Lär dig mer om olika dataformat: [JSON](https://www.json.org/json-en.html), [XML](https://www.w3.org/XML/)
