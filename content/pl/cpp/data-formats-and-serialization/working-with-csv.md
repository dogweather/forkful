---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:41.119953-07:00
description: "Jak to zrobi\u0107: #."
lastmod: '2024-03-13T22:44:35.735194-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:


### Czytanie pliku CSV za pomocą Standardowej Biblioteki C++:
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
        
        // Tutaj przetwarzaj parsedRow
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Pisanie do pliku CSV:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
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

### Używanie biblioteki zewnętrznej: `csv2`:
Chociaż Standardowa Biblioteka C++ dostarcza podstawowych narzędzi do pracy z plikami i łańcuchami znaków, wykorzystanie bibliotek zewnętrznych może upraszczać przetwarzanie CSV. Jedną z takich bibliotek jest `csv2`, znana z łatwości użytkowania i wydajności.

- Instalacja: Zwykle instalowana za pomocą menedżerów pakietów takich jak Conan lub bezpośrednio z jej repozytorium na GitHubie.

Przykład użycia `csv2` do czytania pliku CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // Wydrukuj wartość każdej komórki
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Przykładowy wynik operacji czytania może wyglądać tak (zakładając prosty trzykolumnowy plik CSV):

```
John    29    New York    
Jane    34    Los Angeles
```

Te przykłady mają na celu pokrycie podstawowych operacji CSV w C++. W przypadku bardziej złożonych scenariuszy, takich jak radzenie sobie z dużymi plikami lub złożonymi transformacjami danych, dalsze eksploracje specjalistycznych bibliotek lub narzędzi mogą być uzasadnione.
