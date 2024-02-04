---
title:                "Praca z plikami CSV"
date:                  2024-02-03T19:20:42.145690-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z CSV (wartości rozdzielone przecinkami) obejmuje odczytywanie z i zapisywanie danych do plików CSV, co jest powszechnym formatem przechowywania tabelarycznych danych w formie tekstowej. Programiści manipulują plikami CSV, aby łatwo wymieniać dane między różnymi aplikacjami, bazami danych lub ułatwić zadania przetwarzania i analizy danych.

## Jak to zrobić:

Kotlin, będąc statycznie typowanym językiem programowania działającym na JVM, nie zawiera wbudowanej biblioteki do obsługi plików CSV. Możesz jednak użyć klas `BufferedReader` i `FileWriter` z Javy do podstawowych operacji lub skorzystać z popularnych bibliotek stron trzecich, takich jak `kotlinx.serialization` i `opencsv`, dla bardziej zaawansowanej funkcjonalności.

### Odczyt pliku CSV za pomocą BufferedReader:

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

_Przykładowe wyjście:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Zapis do pliku CSV za pomocą FileWriter:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { wiersz ->
            writer.write(wiersz.joinToString(",") + "\n")
        }
    }
}
```

To utworzy lub zastąpi `output.csv` podanymi danymi.

### Używanie kotlinx.serialization do serializacji CSV:

Najpierw dodaj zależność do swojego `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Uwaga: Upewnij się, że masz poprawną wersję i konfigurację repozytorium._

Następnie, zdefiniuj swoją klasę danych i użyj formatu `Csv` do serializacji:

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

_Przykładowe wyjście:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Używanie OpenCSV do zaawansowanych operacji:

Dodaj OpenCSV do zależności swojego projektu:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Odczytywanie i zapisywanie za pomocą OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Odczytywanie CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // Zapisywanie CSV
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

Te fragmenty kodu demonstrują elastyczność, jaką Kotlin oferuje przy pracach z plikami CSV, pozwalając wybrać metodę, która najlepiej pasuje do potrzeb twojego projektu.
