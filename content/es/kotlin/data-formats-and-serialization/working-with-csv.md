---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:24.200008-07:00
description: "Trabajar con CSV (Valores Separados por Comas) implica leer y escribir\
  \ datos en archivos CSV, un formato com\xFAn para almacenar datos tabulares en texto\u2026"
lastmod: '2024-03-13T22:44:59.058822-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con CSV (Valores Separados por Comas) implica leer y escribir datos\
  \ en archivos CSV, un formato com\xFAn para almacenar datos tabulares en texto\u2026"
title: Trabajando con CSV
weight: 37
---

## ¿Qué y por qué?

Trabajar con CSV (Valores Separados por Comas) implica leer y escribir datos en archivos CSV, un formato común para almacenar datos tabulares en texto plano. Los programadores manipulan archivos CSV para intercambiar fácilmente datos entre diferentes aplicaciones, bases de datos o para facilitar tareas de procesamiento y análisis de datos.

## Cómo:

Kotlin, siendo un lenguaje de programación de tipos estáticos que se ejecuta en la JVM, no incluye una biblioteca integrada para manejar archivos CSV. Sin embargo, puedes utilizar las clases `BufferedReader` y `FileWriter` de Java para operaciones básicas, o aprovechar bibliotecas de terceros populares como `kotlinx.serialization` y `opencsv` para una funcionalidad más avanzada.

### Leer un archivo CSV usando BufferedReader:

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

_Salida de muestra:_

```
[Nombre, Edad, Ciudad]
[John Doe, 30, Nueva York]
[Jane Smith, 25, Londres]
```

### Escribir en un archivo CSV usando FileWriter:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Nombre", "Edad", "Ciudad"),
        listOf("John Doe", "30", "Nueva York"),
        listOf("Jane Smith", "25", "Londres")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

Esto creará o sobrescribirá `output.csv` con los datos proporcionados.

### Usar kotlinx.serialization para la serialización de CSV:

Primero, agrega la dependencia a tu `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Nota: Asegúrate de tener la versión correcta y la configuración del repositorio._

Luego, define tu clase de datos y usa el formato `Csv` para la serialización:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Persona(val nombre: String, val edad: Int, val ciudad: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Persona("John Doe", 30, "Nueva York"),
        Persona("Jane Smith", 25, "Londres")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_Salida de muestra:_

```
John Doe,30,Nueva York
Jane Smith,25,Londres
```

### Usar OpenCSV para operaciones avanzadas:

Agrega OpenCSV a las dependencias de tu proyecto:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Lectura y escritura con OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Leyendo CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // Escribiendo CSV
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Nombre", "Edad", "Ciudad"),
            arrayOf("John Doe", "30", "Nueva York"),
            arrayOf("Jane Smith", "25", "Londres")
        )
        csvWriter.writeAll(entries)
    }
}
```

Estos fragmentos de código demuestran la flexibilidad que Kotlin ofrece al trabajar con archivos CSV, permitiéndote elegir el método que mejor se adapte a las necesidades de tu proyecto.
