---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV (Comma Separated Values) er et enkelt filformat for tabellinformasjon. Programmerere bruker CSV for å enkelt eksportere og importere data, som kan leses av nesten hvilken som helst programvare, inkludert regneark og databaser.

## Hvordan gjøre det:
For å jobbe med CSV i Kotlin, kan du bruke standard I/O klasser eller biblioteker som `kotlin-csv`.

```Kotlin
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader

fun lesCsvFil(filbane: String) {
    val rader = csvReader().readAll(File(filbane))
    for (rad in rader) {
        println(rad.joinToString(", "))
    }
}

fun main() {
    lesCsvFil("data.csv")
}
```

Forventet utskrift for en CSV-fil med navn `data.csv`:
```
Navn, Alder, By
Ola, 25, Oslo
Kari, 30, Bergen
```

## Dypdykk
CSV-formatet stammer fra tidlig datamaskinbruk (1970-tallet) og har siden vært en enkel måte å utveksle data på. Til tross for mangel på standardisering, er filformatet populært grunnet sin enkelhet. Alternativer inkluderer JSON og XML, som begge tillater mer kompleks datastrukturer. Detaljer rundt implementering i Kotlin kan variere, men mange bruker biblioteker som `kotlin-csv` for å forenkle prosessen.

## Se også
- [kotlin-csv GitHub-side](https://github.com/doyaaaaaken/kotlin-csv)
- [Offisiell Kotlin-dokumentasjon](https://kotlinlang.org/docs/home.html)
