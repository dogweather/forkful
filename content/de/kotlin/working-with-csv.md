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

Was ist CSV und warum verwenden Programmierer es?

CSV steht für "Comma Separated Values" und ist ein universales Dateiformat zum Speichern von tabellarischen Daten. Programmierer verwenden es, um Daten in eine einfach lesbare und bearbeitbare Struktur zu bringen.

Wie geht das?

Kotlin bietet verschiedene Methoden, um mit CSV-Dateien zu arbeiten. Das CSV-Library von JetBrains ist eine gut dokumentierte und leicht zu verwendende Option.

1. CSV-Datei lesen:

Um eine CSV-Datei zu lesen, wird zunächst ein Reader-Objekt erstellt, das die CSV-Datei als Parameter erhält. Dann kann die Datenzeile für Zeile ausgelesen werden.

```Kotlin
val reader = CSVReader(FileReader("meine_datei.csv"))
val csvData = reader.readAll() // Liste aller Datenzeilen
for (data in csvData) {
    println(data) // Aufruf der einzelnen Datenzeilen
}
```

2. CSV-Datei schreiben:

Auch das Schreiben in eine CSV-Datei ist mit der CSV-Library von Kotlin möglich. Hier wird ein Writer-Objekt erstellt, dem die CSV-Datei und die zu schreibenden Daten übergeben werden. Anschließend können die Daten zeilenweise in die Datei geschrieben werden.

```Kotlin
val writer = CSVWriter(FileWriter("meine_neue_datei.csv"))
val data = arrayOf("Spalte 1", "Spalte 2") // Daten, die geschrieben werden sollen
writer.writeNext(data) // Schreiben der Daten in die CSV-Datei
```

## Vertiefung

- Historischer Kontext: CSV wurde bereits 1972 entwickelt und ist eines der ältesten Dateiformate. Es war ursprünglich für die Speicherung von Daten in Tabellenkalkulationsprogrammen gedacht.
- Alternativen: Es gibt auch andere Formate wie JSON oder XML, aber CSV ist aufgrund seiner einfachen Struktur und der Vielzahl an unterstützten Programmen immer noch eines der beliebtesten Formate.
- Implementierungsdetails: CSV besteht aus einfachen Textdateien, in denen die Daten durch Trennzeichen wie Kommas oder Semikolons voneinander getrennt sind. Die erste Zeile enthält in der Regel die Spaltenüberschriften.

## Siehe auch

- [CSV-Format Spezifikation](https://tools.ietf.org/html/rfc4180)