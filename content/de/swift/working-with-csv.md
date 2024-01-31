---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, kurz für "Comma-Separated Values", ist ein einfaches Dateiformat zum Speichern tabellarischer Daten. Programmierer nutzen CSV, weil es weit verbreitet, menschenlesbar und einfach zu importieren sowie zu exportieren ist.

## How to:
Swift macht das Lesen und Schreiben von CSV-Dateien unkompliziert. Hier ein Beispiel, wie man eine CSV-Datei einliest, verarbeitet und wieder schreibt:

```Swift
import Foundation

// Angenommen, wir haben eine String-Variable `csvContent`, die unsere CSV-Daten enthält
let csvContent = """
Name,Alter,Beruf
Max Mustermann,42,Entwickler
Erika Mustermann,36,Designerin
"""

// Parsing der CSV Daten
func parseCSV(content: String) -> [[String]] {
    var result: [[String]] = []
    let rows = content.components(separatedBy: "\n")
    
    for row in rows {
        let columns = row.components(separatedBy: ",")
        result.append(columns)
    }
    
    // Der erste Eintrag enthält die Spaltennamen, könnte je nach Bedarf entfernt werden
    return result
}

// CSV-String in ein Array von Arrays konvertieren
let parsedCSV = parseCSV(content: csvContent)
print(parsedCSV)

// CSV-Daten schreiben
func writeCSV(data: [[String]]) -> String {
    var content = ""
    
    for (index, row) in data.enumerated() {
        let rowString = row.joined(separator: ",")
        
        content += rowString
        // Zeilenumbruch, außer nach der letzten Zeile
        if index < data.count - 1 {
            content += "\n"
        }
    }
    
    return content
}

// Erzeugten CSV-String für die Ausgabe oder Speicherung
let csvOutput = writeCSV(data: parsedCSV)
print(csvOutput)
```

Ausgabe beim Einlesen und Schreiben der CSV-Daten zeigt identischen Inhalt wie `csvContent`.

## Deep Dive
CSV ist seit den frühen Computerjahren in Verwendung. Es gibt zwar modernere Alternativen wie JSON oder XML, die mehr Datenstruktur und Meta-Informationen bieten, doch CSV bleibt wegen seiner Einfachheit beliebt. Bei der Implementierung in Swift ist zu beachten, dass das Handling von Komplikationen, wie Zitate oder Kommas innerhalb von Zellen, zusätzliche Logik erfordert.

## See Also
Weitere Infos und Tutorials für Swift-Programmierung und CSV-Handhabung:

- Offizielle Swift-Dokumentation: [swift.org/documentation/](https://swift.org/documentation/)
- CSV-Spezifikation von RFC 4180: [tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Swift-Standardbibliothek: [developer.apple.com/documentation/swift](https://developer.apple.com/documentation/swift)
- Tutorial zu Swift und CSV mit Bibliotheken: [raywenderlich.com/567-urlsession-tutorial-getting-started](https://www.raywenderlich.com/567-urlsession-tutorial-getting-started)
