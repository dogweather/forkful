---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
simple_title:         "Manipulation des fichiers CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
CSV, c'est texte simplifié pour stocker des données en tableau. Les programmeurs l'utilisent pour échanger des données facilement entre différents systèmes.

## Comment faire :
```Swift
import Foundation

// Exemple : Lire un fichier CSV depuis le système de fichiers
let cheminFichier = "/chemin/vers/votre/fichier.csv"
if let contenu = try? String(contentsOfFile: cheminFichier, encoding: .utf8) {
    let lignes = contenu.components(separatedBy: "\n")
    for ligne in lignes {
        let valeurs = ligne.components(separatedBy: ",")
        print(valeurs)
    }
}

// Exemple : Écrire des données dans un fichier CSV
let donnees = [
    ["id", "nom", "age"],
    ["1", "Alice", "30"],
    ["2", "Bob", "27"]
]

let contenuCSV = donnees.map { ligne in
    ligne.joined(separator: ",")
}.joined(separator: "\n")

do {
    try contenuCSV.write(toFile: "/chemin/de/sortie/votre/fichier.csv", atomically: true, encoding: .utf8)
    print("Fichier CSV sauvegardé.")
} catch {
    print("Erreur lors de l'écriture du fichier CSV.")
}
```

Sortie de l'exemple d'écriture :
```
Fichier CSV sauvegardé.
```

## Plongée profonde
Les CSV existent depuis les premiers jours de l'ordinateur personnel. Alternatives à CSV incluent JSON, XML – plus structurés, mais plus lourds. La manipulation CSV en Swift n'est pas intégrée ; utiliser `String` pour la lecture/écriture est une solution simple, mais des bibliothèques comme `CodableCSV` ou `SwiftCSV` offrent plus d'options et de sécurité.

## Voir aussi
- Documentation Swift pour `String`: https://developer.apple.com/documentation/swift/string
- CodableCSV, une bibliothèque pour travailler avec des fichiers CSV en Swift: https://github.com/dehesa/CodableCSV
- SwiftCSV, une autre bibliothèque Swift pour CSV: https://github.com/swiftcsv/SwiftCSV
- Guide pratique pour l'import/export CSV en Swift: https://www.raywenderlich.com/3418439-swift-csv-library-getting-started
