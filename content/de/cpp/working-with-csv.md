---
title:                "Arbeiten mit CSV-Dateien"
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values" – das sind Daten, getrennt durch Kommas. Programmierer nutzen CSV, weil es ein simpler, textbasierter Standard ist, der leicht zwischen verschiedenen Programmen getauscht werden kann.

## So geht's:
```C++
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>

int main() {
    std::ifstream csvFile("beispiel.csv");
    std::string line;
    
    while (getline(csvFile, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> rowData;
        
        while (getline(lineStream, cell, ',')) {
            rowData.push_back(cell);
        }

        // Benutzung der rowData...
        for(const auto& data : rowData) {
            std::cout << data << " ";
        }
        std::cout << std::endl;
    }
    return 0;
}
```
Output (angenommen `beispiel.csv` hat zwei Zeilen mit je drei Einträgen):
```
Eintrag1 Eintrag2 Eintrag3 
Eintrag4 Eintrag5 Eintrag6 
```

## Tiefgang
Historisch wurde CSV in den frühen Computerjahren entwickelt, um Daten tabellarisch zu erfassen und zwischen Programmen auszutauschen. Es gibt Alternativen wie JSON oder XML, die mehr Datenstruktur bieten, aber CSV bleibt beliebt wegen seiner Einfachheit und Lesbarkeit. Beim Umgang mit CSV in C++ ist wichtig, dass die Standardbibliothek keine native Unterstützung bietet und man Bibliotheken von Dritten oder eigene Parser schreiben muss.

## Siehe Auch
- **RFC 4180**: Formale Definition von CSV von der IETF: https://tools.ietf.org/html/rfc4180
- **C++ Standardbibliothek**: Dokumentation zum iostream-Modul, das zum Lesen/Schreiben von Dateien verwendet wird: http://www.cplusplus.com/reference/iostream/
- **cppreference.com**: Ein nützliches C++ Referenzhandbuch: https://en.cppreference.com/w/
- **Boost Libraries**: Für fortgeschrittene CSV-Handling: https://www.boost.org/doc/libs/release/libs/tokenizer/
