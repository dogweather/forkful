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

# Qu'est-ce que c'est et pourquoi le faire ?
Travailler avec des fichiers CSV (Comma-Separated Values) est un moyen courant pour les programmeurs de gérer et manipuler des données tabulaires. Les CSV sont faciles à lire pour les humains et peuvent être facilement importés dans des applications comme des feuilles de calcul ou des bases de données. Les programmeurs utilisent les CSV pour stocker des données, telles que des noms, des adresses et des montants, qui peuvent être triées et analysées de manière efficace.

# Comment faire :
La manipulation de fichiers CSV est relativement simple avec Swift. Voici un exemple de code pour ouvrir un fichier CSV, le lire ligne par ligne, et afficher les valeurs séparées par des virgules :

```
import Foundation

let csvFileURL = URL(fileURLWithPath: "sample.csv") // Définir l'URL du fichier CSV
do {
    let csvData = try String(contentsOf: csvFileURL) // Lire le contenu du fichier CSV en tant que chaîne de caractères
    let csvLines = csvData.components(separatedBy: "\n") // Séparer les lignes du CSV en un tableau
    
    for line in csvLines {
        let values = line.components(separatedBy: ",") // Séparer les valeurs de chaque ligne en un tableau
        print(values) // Afficher les valeurs séparées par des virgules
    }
} catch {
    print(error) // Gérer toute erreur de lecture du fichier CSV
}
```

En utilisant l'exemple ci-dessus, si nous avons un fichier CSV avec les données suivantes :

```
Nom,Prénom,Âge
Dupont,Julie,27
Martin,Eric,35
```

La sortie du programme serait la suivante :

```
["Nom", "Prénom", "Âge"]
["Dupont", "Julie", "27"]
["Martin", "Eric", "35"]
```

# Plongée en profondeur :
Les fichiers CSV ont été développés dans les années 1970 pour stocker des données tabulaires dans les systèmes informatiques. Ils sont devenus un format de fichier courant pour partager des données entre différentes applications et systèmes d'exploitation. En plus de séparer les valeurs par des virgules, les CSV peuvent également utiliser d'autres caractères de séparation, tels que des points-virgules ou des tabulations.

Alors que les fichiers CSV sont faciles à utiliser, ils peuvent avoir des limitations lorsqu'il s'agit de données complexes et structurées. Dans de tels cas, les programmeurs peuvent utiliser d'autres formats de données tels que JSON ou XML. De plus, il existe des librairies et des outils tels que SwiftCSV et CSVParser pour aider à travailler avec des fichiers CSV en Swift.

# Voir aussi :
- [La documentation officielle de Swift sur la manipulation de fichiers](https://docs.swift.org/swift-book/LanguageGuide/WorkingWithFiles.html)
- [SwiftCSV library](https://github.com/naoty/SwiftCSV)
- [CSVParser library](https://github.com/yaslab/CSV.swift)