---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:40.788875-07:00
description: "Die Arbeit mit CSV (Comma-Separated Values, kommagetrennte Werte) umfasst\
  \ das Lesen von und das Schreiben in CSV-Dateien, ein g\xE4ngiges Format zum\u2026"
lastmod: '2024-03-11T00:14:27.763273-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV (Comma-Separated Values, kommagetrennte Werte) umfasst\
  \ das Lesen von und das Schreiben in CSV-Dateien, ein g\xE4ngiges Format zum\u2026"
title: Arbeiten mit CSV
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV (Comma-Separated Values, kommagetrennte Werte) umfasst das Lesen von und das Schreiben in CSV-Dateien, ein gängiges Format zum Speichern von tabellarischen Daten in Klartext. Programmierer manipulieren CSV-Dateien, um Daten einfach zwischen verschiedenen Anwendungen, Datenbanken zu tauschen oder um Datenverarbeitungs- und Analyseaufgaben zu erleichtern.

## Wie:

Kotlin, eine statisch typisierte Programmiersprache, die auf der JVM läuft, enthält keine integrierte Bibliothek zur Verarbeitung von CSV-Dateien. Sie können jedoch die Java-`BufferedReader`- und `FileWriter`-Klassen für grundlegende Operationen verwenden oder beliebte Drittanbieterbibliotheken wie `kotlinx.serialization` und `opencsv` für fortgeschrittenere Funktionen nutzen.

### Eine CSV-Datei mit BufferedReader lesen:

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

_Beispielausgabe:_

```
[Name, Alter, Stadt]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### In eine CSV-Datei mit FileWriter schreiben:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Alter", "Stadt"),
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

Dies wird `output.csv` mit den bereitgestellten Daten erstellen oder überschreiben.

### Verwendung von kotlinx.serialization für die CSV-Serialisierung:

Zuerst fügen Sie die Abhängigkeit zu Ihrer `build.gradle.kts` hinzu:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Hinweis: Stellen Sie sicher, dass Sie die richtige Version und Konfiguration des Repositories haben._

Dann definieren Sie Ihre Datenklasse und verwenden Sie das `Csv`-Format für die Serialisierung:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val alter: Int, val stadt: String)

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

_Beispielausgabe:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Verwendung von OpenCSV für fortgeschrittene Operationen:

Fügen Sie OpenCSV zu den Abhängigkeiten Ihres Projekts hinzu:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Lesen und Schreiben mit OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // CSV lesen
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val einträge = csvReader.readAll()
        einträge.forEach { println(it.toList()) }
    }

    // CSV schreiben
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val einträge = listOf(
            arrayOf("Name", "Alter", "Stadt"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(einträge)
    }
}
```

Diese Code-Schnipsel demonstrieren die Flexibilität, die Kotlin beim Arbeiten mit CSV-Dateien bietet, und ermöglichen es Ihnen, die Methode auszuwählen, die am besten zu den Anforderungen Ihres Projekts passt.
