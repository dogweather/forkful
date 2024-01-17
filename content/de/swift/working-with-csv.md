---
title:                "Die Arbeit mit csv"
html_title:           "Swift: Die Arbeit mit csv"
simple_title:         "Die Arbeit mit csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?

CSV steht für "Comma-Separated Values" und ist ein Dateiformat, das verwendet wird, um strukturierte Daten in Textform zu speichern. Programmierer nutzen CSV, um Daten in einer einfach lesbaren Art und Weise zu organisieren und zu speichern.

## Wie geht's?

```Swift 

// Beispiel einer CSV-Datei
Name,Alter,Stadt
Max,30,Berlin
Anna,25,Hamburg
Peter,40,München

// Lesen einer CSV-Datei
let fileURL = URL(fileURLWithPath: "meineDatei.csv")
let csvString = try String(contentsOf: fileURL)

// Parsen des CSV-Strings
let lines = csvString.components(separatedBy: "\n")
for line in lines {
    let values = line.components(separatedBy: ",")
    // Werte verarbeiten
}

// Schreiben in eine CSV-Datei
let fileURL = URL(fileURLWithPath: "neueDatei.csv")
var csvString = "Name,Alter,Stadt"
csvString.append("\nMax,30,Berlin")
csvString.append("\nAnna,25,Hamburg")
try csvString.write(to: fileURL, atomically: true, encoding: .utf8)

```

## Tief einsteigen

CSV-Dateien sind seit den 1970er Jahren beliebt und werden immer noch häufig verwendet, insbesondere in der Software-Entwicklung und im Datenmanagement. Alternativen zu CSV sind zum Beispiel JSON oder XML, die jedoch oft komplexere Datenstrukturen erlauben. Die Verarbeitung von CSV-Dateien kann mithilfe von CSV-Parser-Libraries wie CSwiftV oder SwiftCSV erleichtert werden.

## Sieh dir auch an

- [CSwiftV](https://github.com/Daniel1of1/CSwiftV)
- [SwiftCSV](https://github.com/naoty/SwiftCSV)