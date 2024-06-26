---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:42.145690-07:00
description: "Jak to zrobi\u0107: Kotlin, b\u0119d\u0105c statycznie typowanym j\u0119\
  zykiem programowania dzia\u0142aj\u0105cym na JVM, nie zawiera wbudowanej biblioteki\
  \ do obs\u0142ugi plik\xF3w CSV.\u2026"
lastmod: '2024-03-13T22:44:35.387587-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, b\u0119d\u0105c statycznie typowanym j\u0119zykiem programowania\
  \ dzia\u0142aj\u0105cym na JVM, nie zawiera wbudowanej biblioteki do obs\u0142ugi\
  \ plik\xF3w CSV."
title: Praca z plikami CSV
weight: 37
---

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
