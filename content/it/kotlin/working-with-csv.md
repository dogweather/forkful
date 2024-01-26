---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con i CSV significa manipolare dati strutturati come testo, separati da virgole. Programmatore li usa perché è formato diffuso per scambi tra sistemi, facile da leggere e scrivere, sia per l'uomo che per la macchina.

## How to:
```kotlin
import java.io.File

fun readCSV(filePath: String): List<List<String>> {
    return File(filePath).useLines { lines ->
        lines.map { it.split(",") }.toList()
    }
}

fun writeCSV(filePath: String, data: List<List<String>>) {
    File(filePath).bufferedWriter().use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(","))
            writer.newLine()
        }
    }
}

// Esempio di utilizzo
val myData = readCSV("dati.csv")
myData.forEach { println(it) }

writeCSV("dati_output.csv", listOf(listOf("nome", "età", "città"), listOf("Mario", "30", "Roma")))
```
Output per `myData.forEach { println(it) }`:
```
[nome, età, città]
[Mario, 30, Roma]
```

## Deep Dive
CSV sta per Comma-Separated Values, formato nato nei primi anni '70. Oggi esistono alternative come JSON o XML che supportano dati più complessi. In Kotlin puoi usare librerie come Apache Commons CSV o kotlinx.serialization per gestire formati vari, ma la standard library è sufficiente per i casi semplici.

## See Also
- Documentazione della Standard Library di Kotlin: [kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Apache Commons CSV: [commons.apache.org/proper/commons-csv/](https://commons.apache.org/proper/commons-csv/)
- kotlinx.serialization, gestire JSON/XML in Kotlin: [github.com/Kotlin/kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
