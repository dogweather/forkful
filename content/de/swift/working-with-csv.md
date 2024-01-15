---
title:                "Arbeiten mit csv"
html_title:           "Swift: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-csv.md"
---

{{< edit_this_page >}}

# Warum

CSV steht für "Comma Separated Values" und ist ein häufig verwendetes Dateiformat zum Speichern von tabellarischen Daten. Es ist besonders nützlich für die Verarbeitung großer Datenmengen, die in einer einfachen, strukturierten Form vorliegen. In diesem Artikel werden wir untersuchen, warum CSV in der Swift-Programmierung nützlich sein kann und wie man damit umgeht.

# Wie geht man vor?

Um mit CSV-Dateien in Swift zu arbeiten, gibt es einige nützliche Bibliotheken wie z.B. "CSwiftV". Wir werden jedoch in diesem Artikel die Verwendung der integrierten "Foundation" Bibliothek untersuchen, da sie bereits alle notwendigen Funktionen für die Arbeit mit CSV-Dateien bietet.

Zunächst müssen wir unsere Datei einlesen und in eine String-Variable speichern. Dazu können wir die Funktion `String(contentsOfFile: String)` verwenden. Um die Daten in eine benutzerfreundlichere Form zu bringen, können wir die String-Methode `components(separatedBy: String)` verwenden, um die Daten anhand des Trennzeichen "," aufzuteilen und in Arrays zu speichern. Ein Beispielcode sieht folgendermaßen aus:

```Swift
// Einlesen der CSV-Datei
let csvString = try String(contentsOfFile: "meineDaten.csv")

// Aufteilen der Daten anhand des Trennzeichens
let rows = csvString.components(separatedBy: ",")

// Beispiel-Ausgabe der Daten
print(rows)
```

Wenn wir die obige Funktion auf eine CSV-Datei mit folgendem Inhalt anwenden würden:

```
Name, Alter, Stadt
Anna, 26, Berlin
Max, 32, Hamburg
Lisa, 18, München
```

würde die Ausgabe folgendermaßen aussehen:

```
["Name", "Alter", "Stadt", "Anna", "26", "Berlin", "Max", "32", "Hamburg", "Lisa", "18", "München"]
```

Das ist jedoch noch nicht sehr benutzerfreundlich. Wir können nun die Einträge besser gruppieren, indem wir eine Schleife verwenden, um jedes Array-Element in ein Dictionary einzufügen:

```Swift
// Leerzeichen-entfernte Kopfzeile (Erste Zeile)
let header = rows[0].replacingOccurrences(of: " ", with: "")

// Vorbereiten eines leeren Arrays für die Ausgabe
var output: [[String: Any]] = []

// Durchlaufen der Arrays und Hinzufügen der Einträge zum Ausgabe-Array
for row in rows[1...] {
    let entry = row.components(separatedBy: ",")
    var dictionary: [String: Any] = [:]

    for (index, value) in entry.enumerated() {
        dictionary[header[index]] = value
    }
    output.append(dictionary)
}

// Beispiel-Ausgabe der Daten
print(output)
```

Die Ausgabe würde jetzt folgendermaßen aussehen:

```Swift
[["Name": "Anna", "Alter": "26", "Stadt": "Berlin"], ["Name": "Max", "Alter": "32", "Stadt": "Hamburg"], ["Name": "Lisa", "Alter": "18", "Stadt": "München"]]
```

# Tiefere Einblicke

Jetzt, wo wir die grundlegenden Funktionen zum Einlesen und Verarbeiten von CSV-Dateien kennengelernt haben, können wir noch einen Schritt weitergehen und direkt mit der Datei interagieren, ohne sie in eine String-Variable zu konvertieren. Die "Foundation" Bibliothek bietet die `CSVReader`-Klasse, mit der wir direkt auf eine CSV-Datei zugreifen können.

Ein Beispielcode sieht folgendermaßen aus:

```Swift
// Importieren der Foundation Bibliothek
import Foundation

// Pfad zur CSV-Datei
let csvFilePath = "meineDaten.csv"

// Öffnen der Datei mit dem CSVReader
if let reader = try? CSVReader.init(fileURL: URL.init(fileURLWithPath: csvFilePath), hasHeaderRow: true) {

    // Schleife zum Durchlaufen der Datei
    while reader.next() != nil {
        // Zugriff auf die einzelnen Spalten der Zeile über die "headerRow" Variable
        let entry = reader.headerRow

        // Beispielhafte Ausgabe einer Spalte
        print(entry["Name"])
    }
}
```

Mit dem `