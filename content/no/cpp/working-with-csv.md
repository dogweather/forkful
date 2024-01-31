---
title:                "Arbeid med CSV"
date:                  2024-01-19
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Arbeid med CSV (Comma Separated Values) involverer lesing og skriving av data i en tekstformat delt med komma. Programmerere bruker CSV fordi det er enkelt, universelt, og kan brukes på tvers av forskjellige programmer.

## How to:
```C++
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

// Enkel funksjon for å lese CSV
std::vector<std::vector<std::string>> readCSV(const std::string& filename) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(filename);
    
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream s(line);
        std::string field;
        std::vector<std::string> row;
        
        while (getline(s, field, ',')) {
            row.push_back(field);
        }
        data.push_back(row);
    }
    
    return data;
}

// Enkel funksjon for å skrive til CSV
void writeCSV(const std::string& filename, const std::vector<std::vector<std::string>>& data) {
    std::ofstream file(filename);
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); ++i) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
}

int main() {
    // Engangskjøring av lese og skrive funksjonene
    const std::string filename = "example.csv";
    std::vector<std::vector<std::string>> data = readCSV(filename);
    writeCSV("output.csv", data);
    
    return 0;
}
```
Sample output (innholdet av `output.csv` vil være identisk med `example.csv`).

## Deep Dive:
CSV oppstod på 1970-tallet som et enkelt tekstformat for datautveksling. Alternativer til CSV inkluderer JSON, XML, og databaser som SQL. Implementeringsdetaljer i C++ kan inkludere bruk av bibliotek som `<fstream>` for filhåndtering, string streams for parsing av rader, og feilhåndtering.

## See Also:
- [C++ filhåndtering (cplusplus.com)](https://www.cplusplus.com/reference/fstream/)
- [RFC 4180 - Standard for CSV files (ietf.org)](https://tools.ietf.org/html/rfc4180)
- [Modern C++: arbeid med JSON (nlohmann json)](https://github.com/nlohmann/json)
