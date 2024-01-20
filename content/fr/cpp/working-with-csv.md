---
title:                "Travailler avec les fichiers CSV"
html_title:           "C++: Travailler avec les fichiers CSV"
simple_title:         "Travailler avec les fichiers CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

CSV (Comma-Separated Values) est un format de fichier couramment utilisé pour stocker et échanger des données tabulaires. En tant que programmeurs, nous travaillons souvent avec des fichiers CSV car ils sont faciles à lire et à écrire, et qu'ils peuvent être ouverts dans de nombreux programmes tels que Microsoft Excel.

## Comment faire:

Voici un exemple simple de lecture et d'impression d'un fichier CSV en utilisant C++:

```
#include <iostream>
#include <fstream>
#include <string>

int main() {
  std::ifstream fichier("exemple.csv");
  std::string ligne;

  while (getline(fichier, ligne)) { 
    std::cout << ligne << std::endl;
  }
  fichier.close(); 
  return 0; 
}
```

Output:

```
nom,prenom,age
Doe,John,25
Smith,Jane,30
Brown,Mike,40
```

## Plongée en profondeur:

Le format CSV a été créé dans les années 1970 pour faciliter l'échange de données entre programmes. Il est souvent utilisé pour stocker de grandes quantités de données, telles que des bases de données ou des listes de contacts.

Bien qu'il soit largement utilisé et pris en charge par de nombreux programmes et langages de programmation, il existe des alternatives telles que JSON ou XML pour stocker et échanger des données tabulaires. Cependant, CSV reste populaire en raison de sa simplicité et de sa compatibilité.

Pour travailler avec des fichiers CSV en C++, vous pouvez utiliser des bibliothèques tierces telles que [csv-parser] (https://github.com/victronenergy/csv-parser), qui offrent des fonctionnalités supplémentaires telles que la conversion de données en structures C++ ou la manipulation de colonnes spécifiques.

## Voir aussi:

- [Tutorialspoint: Working with CSV files in C++](https://www.tutorialspoint.com/working-with-c-plus-plus-csv-files)