---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:26.999899-07:00
description: "Die Arbeit mit CSV (Comma Separated Values)-Dateien dreht sich um die\
  \ Verarbeitung und Manipulation von Daten, die in einem einfachen Textformat\u2026"
lastmod: '2024-03-13T22:44:54.206733-06:00'
model: gpt-4-0125-preview
summary: Die Arbeit mit CSV (Comma Separated Values)-Dateien dreht sich um die Verarbeitung
  und Manipulation von Daten, die in einem einfachen Textformat gespeichert sind,
  wobei jede Zeile des Textes eine Zeile in einer Tabelle darstellt und Kommas die
  einzelnen Spalten trennen.
title: Arbeiten mit CSV
weight: 37
---

## Wie geht das:


### Eine CSV-Datei mit der C++ Standardbibliothek lesen:
```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // Verarbeitung von parsedRow hier
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### In eine CSV-Datei schreiben:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Alter", "Stadt"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Verwendung einer Drittanbieterbibliothek: `csv2`:
Während die C++ Standardbibliothek die grundlegenden Werkzeuge für die Arbeit mit Dateien und Zeichenketten bietet, kann die Nutzung von Drittanbieterbibliotheken die CSV-Verarbeitung vereinfachen. Eine solche Bibliothek ist `csv2`, bekannt für ihre Benutzerfreundlichkeit und Effizienz.

- Installation: Typischerweise über Paketmanager wie Conan installiert oder direkt aus ihrem GitHub-Repository.

Beispiel für die Verwendung von `csv2` zum Lesen einer CSV-Datei:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // Jeden Zellenwert ausdrucken
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Beispieloutputs für Leseoperationen könnten so aussehen (angenommen wird eine einfache CSV-Datei mit drei Spalten):

```
John    29    New York    
Jane    34    Los Angeles
```

Diese Beispiele sollen grundlegende CSV-Operationen in C++ abdecken. Für komplexere Szenarien, wie den Umgang mit großen Dateien oder komplizierten Datentransformationen, könnte eine weitere Erkundung spezialisierter Bibliotheken oder Werkzeuge angebracht sein.
