---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, eli pilkulla erotettujen arvojen tiedostot ovat helppo tapa tallentaa taulukkomuotoista tietoa. Niitä käytetään datan siirtämiseen, analysointiin ja tallentamiseen, koska niiden rakenne on yksinkertainen ja ne ovat inhimillisesti luettavissa.

## How to:
Kotlinilla voit lukea ja kirjoittaa CSV-tiedostoja peruskirjastoilla. Tässä yksinkertainen esimerkki:

```Kotlin
import java.io.File

fun main() {
    val csvData = "id,name,age\n1,John,23\n2,Jane,28"
    val fileName = "example.csv"

    // Kirjoittaminen tiedostoon
    File(fileName).writeText(csvData)

    // Lukeminen tiedostosta
    val lines = File(fileName).readLines()
    lines.drop(1).forEach { line ->
        val (id, name, age) = line.split(",")
        println("ID: $id, Name: $name, Age: $age")
    }
}
```

Tulostus:
```
ID: 1, Name: John, Age: 23
ID: 2, Name: Jane, Age: 28
```

## Deep Dive
CSV on ollut käytössä jo vuosikymmeniä, ja se on standardi tapa jakaa ja säilöä rajoitettu määrä yksinkertaista, lomakepohjaista tietoa. Vaihtoehtoja ovat mm. JSON ja XML, jotka mahdollistavat monimutkaisempien tietorakenteiden tallennuksen. Kotlinin tapauksessa CSV-tiedostoja käsitellessä voimme hyödyntää myös ulkopuolisia kirjastoja, kuten `kotlin-csv`, jos tarvitsemme lisäominaisuuksia.

## See Also
- Kotlinin virallinen oppaat: https://kotlinlang.org/docs/home.html
- kotlin-csv-kirjasto GitHubissa: https://github.com/doyaaaaaken/kotlin-csv
- CSV-määrittely RFC4180: https://tools.ietf.org/html/rfc4180
