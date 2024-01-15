---
title:                "Arbeiten mit CSV"
html_title:           "Kotlin: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma-Separated Values) ist ein beliebtes Dateiformat für den Austausch von Daten zwischen verschiedenen Programmen. Das Arbeiten mit CSV-Dateien kann hilfreich sein, um Daten effizient zu organisieren und zu analysieren. Mit Kotlin können Sie mithilfe von Bibliotheken wie 'Apache Commons CSV' und Funktionen wie 'split()' CSV-Dateien einfach verarbeiten.

## Wie man es tut

Um mit CSV in Kotlin zu arbeiten, müssen Sie zuerst die entsprechenden Bibliotheken importieren. Hier ist ein Beispiel, um eine CSV-Datei zu lesen und die Daten in eine Liste zu speichern:

```Kotlin
import org.apache.commons.csv.CSVFormat
import java.io.FileReader

val reader = FileReader("meine_datei.csv")
val records = CSVFormat.DEFAULT.parse(reader).records

for (record in records) {
    // Hier können Sie auf die Daten in jeder Zeile zugreifen 
    // record[0] ist die erste Spalte, record[1] die zweite usw.
}
```

Um eine CSV-Datei zu schreiben, können Sie diese Funktion verwenden:

```Kotlin
import org.apache.commons.csv.CSVFormat
import java.io.FileWriter

val writer = FileWriter("output.csv")
val records = listOf(listOf("Spalte 1", "Spalte 2", "Spalte 3"), 
                     listOf("Wert 1", "Wert 2", "Wert 3"))
CSVFormat.DEFAULT.print(writer).use { printer ->
    for (record in records) {
        printer.printRecord(record)
    }
}
```

Dieses Beispiel liest eine Liste von List-Objekten und schreibt sie als Zeilen in die CSV-Datei.

## Tauchen wir tiefer ein

CSV-Dateien folgen einem bestimmten Format, bei dem Spalten durch Kommas getrennt sind und jede Zeile eine neue Datenreihe darstellt. Es gibt jedoch auch verschiedene Konventionen innerhalb dieses Formats, wie z.B. die Verwendung von Anführungszeichen um Textfelder oder die Verwendung von unterschiedlichen Trennzeichen wie Tabs oder Semikolons.

Um mit diesen Unterschieden umzugehen, können Sie bei der Verwendung von 'Apache Commons CSV' verschiedene Optionen angeben, wie z.B. das Trennzeichen, die Anführungszeichen und die Zeichenkodierung.

Es ist auch wichtig zu beachten, dass CSV-Dateien keine Datentypen speichern. Alle Daten werden als Zeichenfolgen gelesen und müssen daher von Ihnen in den richtigen Datentyp konvertiert werden, z.B. in Zahlen oder Booleans.

Es gibt auch andere nützliche Bibliotheken für die Arbeit mit CSV in Kotlin, wie z.B. die 'kotlin-csv' Bibliothek, die eine einfachere und modernere Schnittstelle bietet.

## Siehe auch

- [Offizielle Kotlin-Dokumentation für CSV](https://kotlinlang.org/docs/reference/io.html#working-with-files)
- [Apache Commons CSV-Bibliothek](https://commons.apache.org/proper/commons-csv/)
- [Kotlin-CSV-Bibliothek](https://github.com/doyaaaaaken/kotlin-csv)