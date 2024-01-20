---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Manipolare file CSV significa lavorare con dati in formato "Comma-Separated Values", utile per import/export dati. Programmatori lo fanno per interoperabilità con fogli di calcolo e applicazioni di database.

## How to:
Ecco un esempio di lettura di un file CSV:

```C++
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("esempio.csv");

    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            std::stringstream linestream(line);
            std::string cell;
            std::vector<std::string> row;
            while (getline(linestream, cell, ',')) {
                row.push_back(cell);
            }
            // Usare row come necessario
        }
        file.close();
    } else {
        std::cerr << "Errore nell'apertura del file." << std::endl;
    }
    return 0;
}
```

Scrittura di un file CSV:

```C++
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::vector<std::vector<std::string>> data = {
        {"Nome", "Cognome", "Età"},
        {"Mario", "Rossi", "30"},
        {"Luca", "Bianchi", "25"}
    };

    std::ofstream file("output.csv");

    if (file.is_open()) {
        for (const auto& row : data) {
            for (size_t i = 0; i < row.size(); i++) {
                file << row[i];
                if (i < row.size() - 1) file << ',';
            }
            file << '\n';
        }
        file.close();
    } else {
        std::cerr << "Errore nella scrittura del file." << std::endl;
    }
    return 0;
}
```

## Deep Dive
L'uso di CSV risale agli anni '70. Alternativa moderna è JSON o XML, più strutturati. In C++, librarie come `<fstream>` e `<sstream>` permettono di gestire CSV. Attenzione però al parsing di CSV: se i dati contengono virgole o ritorni a capo, occorre gestire le eccezioni.

## See Also
- Documentazione ufficiale di C++ `<fstream>`: https://en.cppreference.com/w/cpp/io/basic_fstream
- RFC 4180 su CSV: https://tools.ietf.org/html/rfc4180
- Libreria per gestione avanzata di CSV in C++: https://github.com/vincentlaucsb/csv-parser