---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:49.754023-07:00
description: "CSV:n (pilkulla erotetut arvot) k\xE4sittelyyn kuuluu tietojen lukeminen\
  \ ja kirjoittaminen CSV-tiedostoihin, jotka ovat yleinen muoto taulukoiden tietojen\u2026"
lastmod: '2024-03-11T00:14:30.499559-06:00'
model: gpt-4-0125-preview
summary: "CSV:n (pilkulla erotetut arvot) k\xE4sittelyyn kuuluu tietojen lukeminen\
  \ ja kirjoittaminen CSV-tiedostoihin, jotka ovat yleinen muoto taulukoiden tietojen\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV:n (pilkulla erotetut arvot) käsittelyyn kuuluu tietojen lukeminen ja kirjoittaminen CSV-tiedostoihin, jotka ovat yleinen muoto taulukoiden tietojen tallentamiseen pelkkänä tekstinä. Ohjelmoijat manipuloivat CSV-tiedostoja helpottaakseen tietojen vaihtoa eri sovellusten, tietokantojen välillä tai helpottaakseen tietojenkäsittely- ja analysointitehtäviä.

## Miten:

Kotlin, ollessaan staattisesti tyypitetty ohjelmointikieli, joka toimii JVM:llä, ei sisällä sisäänrakennettua kirjastoa CSV-tiedostojen käsittelyyn. Voit kuitenkin käyttää Javan `BufferedReader`- ja `FileWriter`-luokkia perusoperaatioihin tai hyödyntää suosittuja kolmannen osapuolen kirjastoja, kuten `kotlinx.serialization` ja `opencsv`, monimutkaisempaan toiminnallisuuteen.

### CSV-tiedoston lukeminen BufferedReaderin avulla:

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

_Esimerkkituloste:_

```
[Nimi, Ikä, Kaupunki]
[John Doe, 30, New York]
[Jane Smith, 25, Lontoo]
```

### Kirjoittaminen CSV-tiedostoon FileWriterin avulla:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Nimi", "Ikä", "Kaupunki"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "Lontoo")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

Tämä luo tai korvaa `output.csv` tiedoston annetuilla tiedoilla.

### Käyttäen kotlinx.serializationia CSV-serialisointiin:

Lisää ensin riippuvuus `build.gradle.kts`-tiedostoosi:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Huom: Varmista, että sinulla on oikea versio ja repositoorion konfiguraatio._

Määritä sitten dataluokkasi ja käytä `Csv` formaattia serialisointiin:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Henkilö(val nimi: String, val ikä: Int, val kaupunki: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Henkilö("John Doe", 30, "New York"),
        Henkilö("Jane Smith", 25, "Lontoo")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_Esimerkkituloste:_

```
John Doe,30,New York
Jane Smith,25,Lontoo
```

### Käyttäen OpenCSV:tä monimutkaisempiin operaatioihin:

Lisää OpenCSV riippuvuudeksi projektisi:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Lukeminen ja kirjoittaminen OpenCSV:llä:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // CSV:n lukeminen
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val merkinnät = csvReader.readAll()
        merkinnät.forEach { println(it.toList()) }
    }

    // CSV:n kirjoittaminen
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val merkinnät = listOf(
            arrayOf("Nimi", "Ikä", "Kaupunki"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "Lontoo")
        )
        csvWriter.writeAll(merkinnät)
    }
}
```

Nämä koodinpätkät osoittavat Kotlinin joustavuuden työskennellessä CSV-tiedostojen kanssa, antaen sinulle mahdollisuuden valita projektiisi parhaiten sopivan menetelmän.
