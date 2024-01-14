---
title:                "Kotlin: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien sind ein gängiges Format, um Daten in einer einfachen und strukturierten Art und Weise zu speichern. Sie werden häufig in der Softwareentwicklung verwendet, um Informationen zu importieren, exportieren oder zu analysieren. Mit Kotlin können wir CSV-Dateien effizient verarbeiten und manipulieren, was uns Zeit und Aufwand sparen kann.

## Wie man es macht

Um mit CSV-Dateien in Kotlin zu arbeiten, können wir die Java-Bibliothek "OpenCSV" verwenden. Zunächst müssen wir die Abhängigkeit in unserem Projekt hinzufügen:

```Kotlin
dependencies {
    implementation("com.opencsv", name = "opencsv", version = "5.2")
}
```

Als nächstes erstellen wir eine Klasse, die unsere CSV-Daten darstellt:

```Kotlin
class Person(val name: String, val age: Int, val city: String)
```

Um eine CSV-Datei zu lesen, erstellen wir eine Instanz von CSVReader und geben den Dateipfad als Parameter an:

```Kotlin
val reader = CSVReader(FileReader("path/to/file.csv"))
```

Dann können wir die Zeilen der Datei mithilfe der `readAll()` Methode auslesen und in einer Liste speichern:

```Kotlin
val rows = reader.readAll()
```

Jede Zeile wird als `Array<String>` zurückgegeben, wobei jedes Element eine Spalte in der CSV-Datei darstellt. Wir können dann durch diese Liste iterieren und unsere Daten in Objekte umwandeln:

```Kotlin
val persons = rows.map { row ->
    Person(row[0], row[1].toInt(), row[2])
}
```

Um eine CSV-Datei zu schreiben, erstellen wir eine Instanz von CSVWriter und geben den Dateipfad und den Trennzeichen als Parameter an:

```Kotlin
val writer = CSVWriter(FileWriter("path/to/file.csv"), ',')
```

Um Daten zu unserer CSV-Datei hinzuzufügen, nutzen wir die `writeNext()` Methode und übergeben ein `Array<String>` mit den entsprechenden Werten:

```Kotlin
writer.writeNext(arrayOf("Max Mustermann", "30", "Berlin"))
```

Und abschließend nicht vergessen, den Writer zu schließen:

```Kotlin
writer.close()
```

## Tiefergehende Informationen

Beim Lesen und Schreiben von CSV-Dateien sollten wir einige Dinge beachten. Zum Beispiel müssen wir die Codierung der Datei berücksichtigen, um Sonderzeichen richtig zu verarbeiten. Auch kann es hilfreich sein, beim Schreiben von Daten ein Escape-Zeichen zu definieren, um mögliche Trennzeichen im Text zu vermeiden.

OpenCSV bietet auch weitere Funktionen wie das Lesen von CSV-Daten aus URLs oder InputStreams und die Verwendung von benutzerdefinierten Trennzeichen.

## Siehe auch

- [OpenCSV Dokumentation](http://opencsv.sourceforge.net/)
- [Offizielle Kotlin Website](https://kotlinlang.org/)
- [Kotlin CSV Bibliothek](https://github.com/opencsv/opencsv)