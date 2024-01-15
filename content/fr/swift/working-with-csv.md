---
title:                "Travailler avec les fichiers csv"
html_title:           "Swift: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données tabulaires telles que des feuilles de calcul, il est probable que vous ayez été confronté au format CSV. Ce format est couramment utilisé pour stocker des données dans un format facilement lisible par les machines, ce qui le rend utile pour l'importation et l'exportation de données dans des applications.

## Comment faire

Pour travailler avec des fichiers CSV en Swift, vous pouvez utiliser la librairie standard `CSV` qui est incluse dans le langage. Tout d'abord, il est nécessaire d'importer la librairie avec l'instruction `import Foundation`.

Ensuite, vous pouvez créer un objet `CSVReader` en passant l'emplacement du fichier CSV en tant que paramètre. Par exemple :

```Swift
let csvPath = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent("example.csv")
let csvReader = try! CSVReader.init(contentsOf: csvPath)
```

Une fois que vous avez créé l'objet `CSVReader`, vous pouvez parcourir le contenu du fichier CSV ligne par ligne en appelant la méthode `next()` et en utilisant une boucle while. Par exemple :

```Swift
while let row = csvReader.next() {
  // faire quelque chose avec la ligne
}
```

À chaque itération de la boucle, la méthode `next()` renvoie un tableau de chaînes de caractères représentant les valeurs de chaque colonne dans la ligne actuelle. Vous pouvez ensuite facilement accéder à ces valeurs en utilisant des index ou des noms de colonnes.

Une fois que vous avez terminé de travailler avec le fichier CSV, n'oubliez pas de fermer l'objet `CSVReader` en appelant la méthode `close()`.

## Plongée en profondeur

La librairie `CSV` de Swift prend en charge diverses options pour personnaliser la façon dont les fichiers CSV sont lus. Par exemple, vous pouvez spécifier un délimiteur de colonne différent de la virgule par défaut en utilisant la propriété `delimiter` de l'objet `CSVReader`.

Vous pouvez également utiliser la librairie `CSV` pour écrire des données dans un fichier CSV en utilisant un objet `CSVWriter`. Il suffit de créer l'objet avec l'emplacement du fichier en paramètre et d'utiliser la méthode `writeRow` pour enregistrer une ligne de données.

Enfin, il est important de noter que la librairie `CSV` de Swift ne prend pas en charge les fichiers CSV avec des entêtes de colonnes. Si vous travaillez avec un fichier CSV qui en contient, vous devrez gérer la première ligne séparément.

## Voir aussi

- [Documentation officielle de la librairie CSV de Swift](https://developer.apple.com/documentation/foundation/csvreader)
- [Guide sur l'importation de données CSV en Swift](https://medium.com/@felixsanz/working-with-csv-files-in-swift-589ad89ae2f5)
- [Exemple de manipulation de données CSV en Swift](https://github.com/samratashok/Countries/blob/master/Countries/Page1/DownloadData.swift)