---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Manipulacja danymi CSV (Comma-Separated Values) to częste zadanie w programowaniu – pozwala na obsługę tabelarycznych danych łatwo zapisywanych i odczytywanych. Programiści używają CSV ze względu na jego prostotę i wszechobecność w różnych systemach i aplikacjach.

## Jak to zrobić:
```Swift
import Foundation

// Przykładowe dane w formacie CSV
let csvData = """
id, name, age
1, Alice, 30
2, Bob, 25
3, Charlie, 35
"""

// Funkcja rozdzielająca dane CSV na wiersze i pola
func parseCSV(csv: String) -> [[String]] {
    var result: [[String]] = []
    let rows = csv.components(separatedBy: "\n")
    
    for row in rows {
        let columns = row.components(separatedBy: ", ").map { $0.trimmingCharacters(in: .whitespaces) }
        result.append(columns)
    }
    
    return result
}

// Użycie funkcji
let parsedCSV = parseCSV(csv: csvData)
print(parsedCSV)
```

Wyjście:
```
[["id", "name", "age"], ["1", "Alice", "30"], ["2", "Bob", "25"], ["3", "Charlie", "35"]]
```

## Wiedza w głębi:
CSV został opracowany w latach 70. XX wieku jako prosty format przenoszenia danych tabelarycznych między programami. Alternatywami dla CSV są JSON, XML, lub bazy danych, ale żaden nie dorównuje CSV w prostocie. Implementacja odczytu CSV w Swift może być bardziej zaawansowana, np. obsługa cudzysłowiów czy nowych linii w komórkach, ale podstawowa funkcjonalność jest prosta.

## Zobacz również:
- Specyfikacja CSV (RFC 4180): https://tools.ietf.org/html/rfc4180
- Biblioteka `CodableCSV` do zaawansowanego parsowania i pisania CSV w Swift: https://github.com/dehesa/CodableCSV
- Apple Swift Docs (dokumentacja Swift od Apple): https://docs.swift.org/swift-book/
