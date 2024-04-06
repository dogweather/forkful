---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:06.992077-07:00
description: ''
lastmod: '2024-04-05T21:59:55.869063-06:00'
model: gpt-4-0125-preview
summary: ''
title: Lavorare con i CSV
weight: 37
---

## Come fare:


### Leggere un file CSV utilizzando la Standard Library di C++:
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
        
        // Elabora parsedRow qui
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Scrivere in un file CSV:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> dati = {
        {"Nome", "Età", "Città"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& riga : dati) {
        for (size_t i = 0; i < riga.size(); i++) {
            file << riga[i];
            if (i < riga.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Utilizzare una libreria di terze parti: `csv2`:
Sebbene la Standard Library di C++ fornisca gli strumenti di base per lavorare con file e stringhe, sfruttare le librerie di terze parti può semplificare l'elaborazione dei CSV. Una di queste librerie è `csv2`, conosciuta per la sua facilità d'uso e efficienza.

- Installazione: Tipicamente installata tramite gestori di pacchetti come Conan o direttamente dal suo repository GitHub.

Esempio di utilizzo di `csv2` per leggere un file CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto intestazione = csv.header();
        for (const auto riga : csv) {
            for (const auto cella : riga) {
                std::cout << cella.second << "\t"; // Stampa il valore di ogni cella
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

L'output di esempio per le operazioni di lettura potrebbe apparire così (assumendo un semplice file CSV a tre colonne):

```
John    29    New York    
Jane    34    Los Angeles
```

Questi esempi mirano a coprire le operazioni fondamentali sui CSV in C++. Per scenari più complessi, come il trattamento di file di grandi dimensioni o trasformazioni complesse dei dati, potrebbe essere opportuna un'ulteriore esplorazione di librerie specializzate o strumenti.
