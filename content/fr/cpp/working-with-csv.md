---
title:                "Manipulation des fichiers CSV"
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Manipuler des CSV, c'est jouer avec du texte structuré: valeurs séparées par des virgules, souvent pour des datas. Les dev s'en servent pour importer/exporter des données facilement entre des apps, des bases de données, et pour le traitement de données.

## Comment faire:
```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

// Lecture d'un fichier CSV
void lireCSV(const std::string& fichier) {
    std::ifstream file(fichier);
    std::string ligne;
    while (getline(file, ligne)) {
        std::stringstream ss(ligne);
        std::string donnee;
        while (getline(ss, donnee, ',')) {
            std::cout << donnee << "\t";
        }
        std::cout << std::endl;
    }
}

// Écriture dans un fichier CSV
void ecrireCSV(const std::string& fichier, const std::vector<std::vector<std::string>>& data) {
    std::ofstream file(fichier);
    for(const auto& ligne : data) {
        for(size_t i = 0; i < ligne.size(); ++i) {
            file << ligne[i];
            if (i < ligne.size() - 1) file << ',';
        }
        file << std::endl;
    }
}

int main() {
    // À remplacer par votre chemin de fichier
    const std::string fichierLecture = "data.csv";
    const std::string fichierEcriture = "output.csv";

    lireCSV(fichierLecture);

    std::vector<std::vector<std::string>> data = {
        {"nom", "age", "ville"},
        {"Alice", "24", "Paris"},
        {"Bob", "30", "Lyon"}
    };

    ecrireCSV(fichierEcriture, data);
    return 0;
}
```
Sortie (pour la lecture de "data.csv"):
```
nom   age   ville   
Alice 24    Paris   
Bob   30    Lyon   
```

## Pour aller plus loin:
Le CSV est vieux (années 70), mais tient bon grâce à sa simplicité. Des alternatives modernes existent, comme JSON ou XML, mais CSV reste top pour de gros volumes de données ou l'interopérabilité avec des tableurs. En C++, manipuler des CSV est souvent manuel, mais on peut utiliser des bibliothèques comme `csv-parser` pour plus de fonctionnalités.

## Voir aussi:
- [cppreference.com](https://en.cppreference.com/w/): Documentation C++ standard.
- [GitHub - csv-parser](https://github.com/vincentlaucsb/csv-parser): Une bibliothèque moderne pour lire et écrire des CSV en C++.
- [Stack Overflow](http://stackoverflow.com/): Pour des questions/réponses sur des problèmes précis de codage CSV en C++.