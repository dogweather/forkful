---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z CSV (Comma-Separated Values) to sposób na łatwy import i eksport danych. Programiści używają go, by sprawnie wymieniać dane między różnymi systemami i aplikacjami.

## How to:
CSV w Kotlinie? Łatwizna! Załadujemy plik, dzielimy dane, voilà!

```Kotlin
import java.io.File

fun main() {
    val fileName = "data.csv"
    val lines = File(fileName).readLines()
    lines.forEach { line ->
        val cols = line.split(",")
        println("Pole 1: ${cols[0]}, Pole 2: ${cols[1]}")
    }
}
```

Wynik:
```
Pole 1: Jan, Pole 2: Kowalski
Pole 1: Anna, Pole 2: Nowak
```

Zapis? Prosto i szybko:

```Kotlin
import java.io.File

fun main() {
    val data = listOf(
        arrayOf("Jan", "Kowalski"),
        arrayOf("Anna", "Nowak")
    )
    
    File("output.csv").printWriter().use { out ->
        data.forEach { 
            out.println(it.joinToString(","))
        }
    }
}
```

Stworzy plik `output.csv` z danymi.

## Deep Dive
CSV to retro – prosty format z lat 70. Nie ma standardu, ale jest prostota. Alternatywie? JSON dla struktur, XML dla komplikacji. Kotlin używa bibliotek jak opencsv lub Apache Commons CSV dla zaawansowanej roboty z CSV.

## See Also
- [RFC 4180](https://tools.ietf.org/html/rfc4180) – o CSV, najbliższa rzecz standardu.
- [opencsv](http://opencsv.sourceforge.net/) – biblioteka w Javie do pracy z CSV.
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/) – kolejna biblioteka do CSV.
- [Kotlin Documentation](https://kotlinlang.org/docs/reference/) – oficjalna dokumentacja Kotlina.
