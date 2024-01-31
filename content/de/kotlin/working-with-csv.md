---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-separated values". Es ist ein einfaches Format zum Speichern tabellarischer Daten in Textform. Programmierer nutzen CSV wegen seiner Einfachheit und breiten Kompatibilität für Datenexport und -import in Datenbanken und Tabellenkalkulationen.

## How to:
```Kotlin
import java.io.File

fun main() {
    val csvData = """
        Name,Alter,Beruf
        Max,25,Entwickler
        Erika,29,Designerin
        """.trimIndent()

    File("meineDaten.csv").writeText(csvData)

    val lines = File("meineDaten.csv").readLines()
    lines.drop(1) // Überspringe die Kopfzeile
        .map { it.split(",") }
        .forEach { println("${it[0]} ist ${it[1]} Jahre alt und arbeitet als ${it[2]}.") }
}

// Ausgabe:
// Max ist 25 Jahre alt und arbeitet als Entwickler.
// Erika ist 29 Jahre alt und arbeitet als Designerin.
```

## Deep Dive
CSV wurde in den frühen 1970er Jahren populär und ist seitdem ein standardisiertes Austauschformat, besonders wenn es um schnellen und simplen Datenaustausch geht. Alternativen wie JSON oder XML bieten mehr Struktur, sind aber komplexer. Beim Arbeiten mit CSV in Kotlin sollte man auf richtige Zeichenkodierung (z.B. UTF-8) und korrekte Behandlung von Sonderfällen wie Zeilenumbrüche oder Kommas in den Daten achten.

## See Also
- Kotlin Dokumentation: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- CSV Format Spezifikation (RFC 4180): [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- OpenCSV-Bibliothek für komplexere CSV-Aufgaben: [http://opencsv.sourceforge.net/](http://opencsv.sourceforge.net/)
