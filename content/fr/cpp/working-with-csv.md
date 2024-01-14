---
title:                "C++: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec des fichiers CSV en C++ ?

Si vous êtes développeur C++, vous avez probablement déjà entendu parler de fichiers CSV. Il s'agit d'un format de fichier largement utilisé pour stocker des données tabulaires telles que des feuilles de calcul ou des bases de données. Mais pourquoi travailler avec des fichiers CSV en tant que développeur C++ ? La réponse est simple : il s'agit d'un format de fichier simple et facile à utiliser pour stocker et manipuler des données. Dans ce post, nous allons vous montrer comment travailler avec des fichiers CSV en C++ et plonger dans les détails pour en apprendre davantage sur ce format.

## Comment travailler avec des fichiers CSV en C++ ?

Tout d'abord, vous devez avoir un fichier CSV avec lequel travailler. Pour cet exemple, nous allons utiliser un fichier CSV contenant des informations sur des utilisateurs fictifs. Voici un aperçu du fichier :

```
Nom, Âge, Ville
Sophie, 25, Paris
Antoine, 33, Lyon
Camille, 28, Marseille
```

Pour travailler avec ce fichier en C++, nous allons utiliser la bibliothèque `<fstream>` pour lire et écrire dans le fichier. Voici un exemple de code qui lit le fichier CSV et stocke les données dans un vecteur :

```
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

int main() {
  ifstream fichier("utilisateurs.csv"); // ouverture du fichier
  vector<string> utilisateurs; // création d'un vecteur pour stocker les données

  if (fichier) { // si le fichier est ouvert avec succès
    string ligne;
    while (getline(fichier, ligne)) { // lecture de chaque ligne du fichier
      utilisateurs.push_back(ligne); // ajouter la ligne au vecteur
    }

    for (auto& utilisateur : utilisateurs) { // parcourir le vecteur et afficher les données
      cout << utilisateur << endl;
    }
  } else { // si le fichier n'est pas ouvert avec succès
    cerr << "Erreur lors de l'ouverture du fichier." << endl;
  }

  fichier.close(); // fermeture du fichier
}
```

Voici l'output correspondant :

```
Nom, Âge, Ville
Sophie, 25, Paris
Antoine, 33, Lyon
Camille, 28, Marseille
```

Vous pouvez également écrire dans un fichier CSV en utilisant l'objet `ofstream` de la même manière que nous avons utilisé `ifstream` pour lire le fichier. Vous pouvez également utiliser des bibliothèques tierces telles que [fast-cpp-csv-parser](https://github.com/ben-strasser/fast-cpp-csv-parser) pour simplifier le processus de traitement des fichiers CSV en C++.

## Plongée en profondeur

Maintenant que vous savez comment travailler avec des fichiers CSV en C++, il est important de comprendre la structure de ces fichiers. Les fichiers CSV sont généralement séparés par des virgules, d'où leur nom (CSV signifie Comma-Separated Values). Cependant, vous pouvez également rencontrer des fichiers CSV où les données sont séparées par des points-virgules, des tabulations ou d'autres caractères. De plus, les fichiers CSV peuvent également avoir une entête de colonne pour chaque ensemble de données, comme dans notre exemple précédent.

Il est également important de noter que les fichiers CSV peuvent poser des problèmes lors du traitement de données avec des caractères spéciaux ou des lignes vides. Il est donc recommandé de vérifier et de valider correctement les données avant de les manipuler.

## Voir aussi

- [Documentation de la bibliothèque <fstream> en C++](https://www.cplusplus.com/reference/fstream/)
- [Bibliothèque fast-cpp-csv-parser](https://github.com/ben-strasser/fast-cpp-csv-parser)
- [Qu'est-ce qu'un fichier CSV et comment le lire en C++](https://www.studentstutorial.com/cplusplus/data-file-extract-csv)
- [Comment lire et écrire des fichiers CSV en C++](https://www.techiedelight.com/read-write-csv-files/)