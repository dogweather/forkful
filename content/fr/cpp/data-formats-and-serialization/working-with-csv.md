---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:03.378274-07:00
description: ''
lastmod: '2024-04-05T22:00:10.688543-06:00'
model: gpt-4-0125-preview
summary: ''
title: Travailler avec CSV
weight: 37
---

## Comment faire :


### Lire un fichier CSV en utilisant la bibliothèque standard C++ :
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
        
        // Traiter parsedRow ici
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Écrire dans un fichier CSV :
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Nom", "Âge", "Ville"},
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

### Utiliser une bibliothèque tierce : `csv2` :
Bien que la bibliothèque standard C++ fournisse les outils de base pour travailler avec des fichiers et des chaînes, l'utilisation de bibliothèques tierces peut simplifier le traitement des CSV. Une telle bibliothèque est `csv2`, connue pour sa facilité d'utilisation et son efficacité.

- Installation : Typiquement installée via des gestionnaires de paquets comme Conan ou directement depuis son dépôt GitHub.

Exemple d'utilisation de `csv2` pour lire un fichier CSV :

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // Imprimer chaque valeur de cellule
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Un exemple de sortie pour les opérations de lecture pourrait ressembler à ceci (en supposant un simple fichier CSV à trois colonnes) :

```
John    29    New York    
Jane    34    Los Angeles
```

Ces exemples visent à couvrir les opérations fondamentales sur les CSV en C++. Pour des scénarios plus complexes, comme le traitement de fichiers volumineux ou des transformations de données complexes, une exploration plus approfondie de bibliothèques spécialisées ou d'outils pourrait être justifiée.
