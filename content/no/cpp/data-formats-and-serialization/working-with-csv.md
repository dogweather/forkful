---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:17.270913-07:00
description: 'Hvordan: .'
lastmod: '2024-03-13T22:44:41.122716-06:00'
model: gpt-4-0125-preview
summary: ''
title: Arbeide med CSV
weight: 37
---

## Hvordan:


### Lese en CSV-fil ved hjelp av C++ Standardbiblioteket:
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
        
        // Behandle analyzedRow her
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Skrive til en CSV-fil:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Navn", "Alder", "By"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& rad : data) {
        for (size_t i = 0; i < rad.size(); i++) {
            file << rad[i];
            if (i < rad.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Bruk av et tredjepartsbibliotek: `csv2`:
Mens C++ Standardbiblioteket gir grunnleggende verktøy for arbeid med filer og strenger, kan bruk av tredjepartsbiblioteker forenkle CSV-behandling. Et slikt bibliotek er `csv2`, kjent for sin brukervennlighet og effektivitet.

- Installasjon: Vanligvis installert via pakkehåndterere som Conan eller direkte fra GitHub-repositoriet.

Eksempel på bruk av `csv2` til å lese en CSV-fil:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // Skriv ut hver celleverdi
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Eksempelutdata for leseoperasjoner kan se slik ut (med forbehold om en enkel CSV-fil med tre kolonner):

```
John    29    New York    
Jane    34    Los Angeles
```

Disse eksemplene har som mål å dekke grunnleggende CSV-operasjoner i C++. For mer komplekse scenarioer, som å håndtere store filer eller komplekse datatransformasjoner, kan ytterligere utforskning inn i spesialiserte biblioteker eller verktøy være nødvendig.
