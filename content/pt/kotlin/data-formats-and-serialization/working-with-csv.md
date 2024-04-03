---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:51.747907-07:00
description: "Como Fazer: Kotlin, sendo uma linguagem de programa\xE7\xE3o de tipagem\
  \ est\xE1tica que \xE9 executada na JVM, n\xE3o inclui uma biblioteca embutida para\
  \ o manuseio de\u2026"
lastmod: '2024-03-13T22:44:46.564466-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, sendo uma linguagem de programa\xE7\xE3o de tipagem est\xE1tica\
  \ que \xE9 executada na JVM, n\xE3o inclui uma biblioteca embutida para o manuseio\
  \ de arquivos CSV."
title: Trabalhando com CSV
weight: 37
---

## Como Fazer:
Kotlin, sendo uma linguagem de programação de tipagem estática que é executada na JVM, não inclui uma biblioteca embutida para o manuseio de arquivos CSV. No entanto, você pode usar as classes `BufferedReader` e `FileWriter` do Java para operações básicas, ou aproveitar bibliotecas de terceiros populares como `kotlinx.serialization` e `opencsv` para funcionalidades mais avançadas.

### Lendo um arquivo CSV usando BufferedReader:
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

_Saída de amostra:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Escrevendo em um arquivo CSV usando FileWriter:
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

Isso criará ou sobrescreverá `output.csv` com os dados fornecidos.

### Usando kotlinx.serialization para serialização CSV:
Primeiro, adicione a dependência ao seu `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Nota: Certifique-se de que você tenha a versão correta e a configuração do repositório._

Então, defina sua classe de dados e use o formato `Csv` para serialização:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Pessoa(val nome: String, val idade: Int, val cidade: String)

fun main() {
    val formatoCsv = Csv { delimiter = ',' }
    val data = listOf(
        Pessoa("John Doe", 30, "New York"),
        Pessoa("Jane Smith", 25, "London")
    )

    val dadosCsv = formatoCsv.encodeToString(data)
    println(dadosCsv)
}
```

_Saída de amostra:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Usando OpenCSV para operações avançadas:
Adicione OpenCSV às dependências do seu projeto:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Lendo e escrevendo com OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Lendo CSV
    CSVReader(FileReader("data.csv")).use { leitorCsv ->
        val entradas = leitorCsv.readAll()
        entradas.forEach { println(it.toList()) }
    }

    // Escrevendo CSV
    CSVWriter(FileWriter("output.csv")).use { escritorCsv ->
        val entradas = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        escritorCsv.writeAll(entradas)
    }
}
```

Esses trechos de código demonstram a flexibilidade que Kotlin oferece ao trabalhar com arquivos CSV, permitindo que você escolha o método que melhor se adapta às necessidades do seu projeto.
