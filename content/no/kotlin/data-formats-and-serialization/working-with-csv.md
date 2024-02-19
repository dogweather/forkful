---
aliases:
- /no/kotlin/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:32.093244-07:00
description: "Arbeid med CSV (kommaseparerte verdier) inneb\xE6rer lesing fra og skriving\
  \ til CSV-filer, et vanlig format for lagring av tabul\xE6re data i ren tekst.\u2026"
lastmod: 2024-02-18 23:08:53.874181
model: gpt-4-0125-preview
summary: "Arbeid med CSV (kommaseparerte verdier) inneb\xE6rer lesing fra og skriving\
  \ til CSV-filer, et vanlig format for lagring av tabul\xE6re data i ren tekst.\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Arbeid med CSV (kommaseparerte verdier) innebærer lesing fra og skriving til CSV-filer, et vanlig format for lagring av tabulære data i ren tekst. Programmerere manipulerer CSV-filer for å enkelt utveksle data mellom forskjellige applikasjoner, databaser, eller for å lette oppgaver knyttet til databehandling og analyse.

## Hvordan:

Kotlin, som er et statisk typet programmeringsspråk som kjører på JVM, inkluderer ikke et innebygd bibliotek for håndtering av CSV-filer. Du kan imidlertid bruke Java-klassene `BufferedReader` og `FileWriter` for grunnleggende operasjoner, eller utnytte populære tredjepartsbiblioteker som `kotlinx.serialization` og `opencsv` for mer avansert funksjonalitet.

### Lese en CSV-fil ved hjelp av BufferedReader:

```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val path = "data.csv"
    val br = BufferedReader(FileReader(path))
    br.useLines { lines ->
        lines.forEach { line ->
            val cols = line.split(',')
            println(cols)
        }
    }
}
```

_Eksempel på utdata:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Skrive til en CSV-fil ved hjelp av FileWriter:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

Dette vil opprette eller overskrive `output.csv` med de oppgitte dataene.

### Bruke kotlinx.serialization for CSV-serialisering:

Først, legg til avhengigheten til din `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Notat: Sørg for at du har riktig versjon og konfigurasjon av oppbevaringssted._

Deretter, definér din dataklasse og bruk `Csv`-format for serialisering:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val age: Int, val city: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Person("John Doe", 30, "New York"),
        Person("Jane Smith", 25, "London")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_Eksempel på utdata:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Bruke OpenCSV for avanserte operasjoner:

Legg til OpenCSV i prosjektets avhengigheter:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Lese og skrive med OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Lese CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // Skrive CSV
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(entries)
    }
}
```

Disse kodestykkene demonstrerer fleksibiliteten Kotlin tilbyr når du arbeider med CSV-filer, og lar deg velge metoden som best passer dine prosjektbehov.
