---
title:                "Arbeta med CSV"
html_title:           "Kotlin: Arbeta med CSV"
simple_title:         "Arbeta med CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV är ett sätt för programmerare att läsa och skriva data i ett tabellformat. Det är ett vanligt sätt att hantera stora datamängder och gör det enkelt att importera och exportera data från olika program. 

## Hur Man:

För att läsa data från en CSV-fil i Kotlin, använd följande kod:

```Kotlin
import java.io.FileReader
import com.opencsv.CSVReader

fun main(args: Array<String>) {
    val reader = CSVReader(FileReader("data.csv"))
    val data = reader.readAll()

    for (row in data) {
        for (item in row) {
            println(item)
        }
    }
}
```

För att skriva data till en CSV-fil i Kotlin, använd följande kod:

```Kotlin
import java.io.FileWriter
import com.opencsv.CSVWriter

fun main(args: Array<String>) {
    val writer = CSVWriter(FileWriter("data.csv"))
    val data = arrayOf(arrayOf("Name", "Age", "Location"), arrayOf("John", "25", "Stockholm"), arrayOf("Lisa", "30", "Gothenburg"))

    for (row in data) {
        writer.writeNext(row)
    }

    writer.close()
}
```

## Djupdykning:
CSV står för "Comma Separated Values" och har funnits i många år som ett enkelt format för att lagra data. Innan CSV användes ofta tab-separerade filer, men problem uppstod när texten som skulle sparas innehöll kommatecken. CSV introducerade då kommatecken för att separera värden och har varit ett populärt format sedan dess.

Alternativ till CSV inkluderar JSON och XML, men CSV är fortfarande det enklaste alternativet när det kommer till att hantera och läsa stora datamängder.

I Kotlin kan man också använda biblioteket Apache Commons CSV för att hantera CSV-filer.

## Se Även:
- [Kotlin Dokumentation](https://kotlinlang.org/docs/home.html)
- [Apache Commons CSV](http://commons.apache.org/proper/commons-csv/)
- [Comma Separated Values på Wikipedia](https://sv.wikipedia.org/wiki/Comma-separated_values)