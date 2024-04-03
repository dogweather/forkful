---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:11.292775-07:00
description: "Kuinka tehd\xE4: #."
lastmod: '2024-03-13T22:44:56.887828-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka tehdä:


### CSV-tiedoston lukeminen käyttäen C++ Standard Libraryä:
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
        
        // Käsittele parsedRow tässä
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Kirjoittaminen CSV-tiedostoon:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Nimi", "Ikä", "Kaupunki"},
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

### Kolmannen osapuolen kirjaston käyttö: `csv2`:
Vaikka C++ Standard Library tarjoaa perustyökalut tiedostojen ja merkkijonojen käsittelyyn, kolmannen osapuolen kirjastojen hyödyntäminen voi yksinkertaistaa CSV-käsittelyä. Yksi tällainen kirjasto on `csv2`, joka tunnetaan sen helppokäyttöisyydestä ja tehokkuudesta.

- Asennus: Tyypillisesti asennettu paketinhallintajärjestelmien kuten Conan kautta tai suoraan sen GitHub-repositoriosta.

Esimerkki käyttäen `csv2` lukeakseen CSV-tiedoston:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // Tulosta jokainen solun arvo
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Esimerkki tuloste lukemisoperaatioista voisi näyttää tältä (oletetaan yksinkertainen kolmisarakkeinen CSV-tiedosto):

```
John    29    New York    
Jane    34    Los Angeles
```

Nämä esimerkit pyrkivät kattamaan perustason CSV-operaatiot C++:ssa. Monimutkaisemmissa skenaarioissa, kuten suurten tiedostojen käsittelyssä tai monimutkaisten datan muunnosten kanssa, voi olla tarpeen tutkia erikoistuneempia kirjastoja tai työkaluja.
