---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:44.426738-07:00
description: "Lavorare con CSV (Valori Separati da Virgola) implica la lettura e la\
  \ scrittura di dati su file CSV, un formato comune per lo stoccaggio di dati tabellari\u2026"
lastmod: '2024-03-13T22:44:43.413471-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con CSV (Valori Separati da Virgola) implica la lettura e la scrittura\
  \ di dati su file CSV, un formato comune per lo stoccaggio di dati tabellari\u2026"
title: Lavorare con i CSV
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con CSV (Valori Separati da Virgola) implica la lettura e la scrittura di dati su file CSV, un formato comune per lo stoccaggio di dati tabellari in testo semplice. I programmatori manipolano i file CSV per scambiare facilmente dati tra diverse applicazioni, database o per facilitare compiti di elaborazione e analisi dei dati.

## Come fare:

Kotlin, essendo un linguaggio di programmazione staticamente tipizzato che gira sulla JVM, non include una libreria integrata per la gestione dei file CSV. Tuttavia, è possibile utilizzare le classi Java `BufferedReader` e `FileWriter` per le operazioni di base, o sfruttare popolari librerie di terze parti come `kotlinx.serialization` e `opencsv` per funzionalità più avanzate.

### Leggere un file CSV usando BufferedReader:

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

_Output di esempio:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Scrivere su un file CSV usando FileWriter:

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

Questo creerà o sovrascriverà `output.csv` con i dati forniti.

### Utilizzare kotlinx.serialization per la serializzazione CSV:

Prima, aggiungere la dipendenza al proprio `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Nota: Assicurati di avere la versione corretta e la configurazione del repository._

Poi, definisci la tua classe di dati e usa il formato `Csv` per la serializzazione:

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

_Output di esempio:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Utilizzare OpenCSV per operazioni avanzate:

Aggiungi OpenCSV alle dipendenze del tuo progetto:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Leggere e scrivere con OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Lettura CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // Scrittura CSV
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

Questi frammenti di codice dimostrano la flessibilità che Kotlin offre nel lavoro con i file CSV, consentendo di scegliere il metodo che meglio si adatta alle esigenze del proprio progetto.
