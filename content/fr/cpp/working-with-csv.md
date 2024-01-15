---
title:                "Travailler avec les fichiers csv"
html_title:           "C++: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes amené à traiter des données tabulaires, il est fort probable que vous ayez à travailler avec des fichiers CSV (Comma-Separated Values). Ces fichiers sont couramment utilisés pour stocker des données dans un format facilement lisible par les humains et les machines. Il est donc important de savoir comment les manipuler efficacement dans votre programme.

## Comment faire
Pour travailler avec des fichiers CSV en C++, il existe quelques librairies utiles comme [csv-parser](https://github.com/vincentlaucsb/csv-parser) et [FastCSV](https://github.com/ben-strasser/fast-cpp-csv-parser). Voici un exemple simplifié d'utilisation de la librairie FastCSV :

```
#include <iostream>
#include "csv.hpp" // Incluez la header file de la librairie

int main() {
  io::CSVReader<2> in("fichier.csv"); // Initialisez le lecteur CSV avec le nombre de colonnes
  in.read_header(io::ignore_extra_column, "col1", "col2"); // Ignorez les colonnes supplémentaires
  std::string col1; int col2; // Déclarez les variables où stocker les données

  while(in.read_row(col1, col2)){ // Bouclez à travers toutes les lignes
    std::cout << col1 << " - " << col2 << std::endl; // Faites ce que vous voulez avec les valeurs récupérées
  }

  return 0;
}
```

Votre fichier CSV pourrait ressembler à ceci :

```
col1,col2
"donnée1",1
"donnée2",2
"donnée3",3
```

Et voici l'output de notre programme :

```
donnée1 - 1
donnée2 - 2
donnée3 - 3
```

La librairie csv-parser offre également des fonctionnalités similaires et est également facile d'utilisation. N'hésitez pas à explorer ces librairies et à trouver celle qui vous convient le mieux.

## Plongée en profondeur
Si vous souhaitez en savoir plus sur le fonctionnement interne des librairies de manipulation de fichiers CSV en C++, vous pouvez consulter leur code source et vous familiariser avec les structures et algorithmes utilisés. Vous pouvez également vous intéresser aux normes et spécifications du format CSV pour mieux comprendre comment ces librairies fonctionnent.

## Voir aussi
- [https://fr.wikipedia.org/wiki/CSV](https://fr.wikipedia.org/wiki/CSV) pour en savoir plus sur le format CSV
- [https://docs.microsoft.com/fr-fr/cpp/standard-library/c-filesystem-classes](https://docs.microsoft.com/fr-fr/cpp/standard-library/c-filesystem-classes) pour manipuler efficacement les fichiers en C++

*Cet article a été écrit et traduit en français par un contributeur de la communauté de Codecademy.*