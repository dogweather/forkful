---
title:                "Å jobbe med csv"
html_title:           "Kotlin: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med CSV-filer kan være svært nyttig for å håndtere store mengder data på en strukturert måte. Ved å bruke Kotlin kan man effektivt lese, skrive og manipulere CSV-filer, noe som kan spare mye tid og forenkle datahåndteringsprosessen.

## Hvordan

For å jobbe med CSV-filer i Kotlin, trenger du først å importere "kotlinx-io" biblioteket. Deretter kan du bruke følgende kode for å lese data fra en CSV-fil:

```
import kotlinx.io.core.*
import kotlinx.serialization.*
import kotlinx.serialization.csv.*

val csvFile = File("path/to/file.csv")
val csvContent = csvFile.readText()
val csv = Csv { delimiter = ';' }
val csvRecords = csv.readRecords(csvContent)
```

Dette vil gi deg en liste med rader fra CSV-filen, som hver kan aksesseres ved hjelp av indeksering. For eksempel:

```
val firstRow = csvRecords[0]
println(firstRow[1]) // Gir deg andre kolonne av første rad
```

For å skrive data til en ny CSV-fil, kan du bruke følgende kode:

```
val newCSVFile = File("path/to/newFile.csv")
csv.write(newCSVFile) {
    writeRow("Navn", "Alder")
    writeRow("Anne", 30)
    writeRow("Ole", 28)
}
```

Dette vil opprette en ny CSV-fil med navn og alder i hver rad.

## Dypdykk

Kotlin tilbyr også muligheten til å definere egendefinerte Serializers for komplekse CSV-strukturer. Dette gjøres ved å implementere CsvInput og CsvOutput grensesnittene. Det finnes også flere tredjepartsbiblioteker for å gjøre arbeidet med CSV-filer enda enklere i Kotlin, som for eksempel "Kotlin-CSV" og "Kotlin-Serialization-CSV".

## Se også

- [Kotlinx-io biblotek](https://github.com/Kotlin/kotlinx-io)
- [Kotlin-CSV tredjepartsbibliotek](https://github.com/doyaaaaaken/kotlin-csv)
- [Kotlin-Serialization-CSV tredjepartsbibliotek](https://github.com/doyaaaaaken/kotlin-csv/tree/master/kotlinx-serialization-csv)