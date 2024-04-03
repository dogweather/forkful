---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:43.449643-07:00
description: "Comment faire : En Swift, il n'y a pas de prise en charge native pour\
  \ l'analyse directe des fichiers CSV, mais vous pouvez g\xE9rer les donn\xE9es CSV\
  \ en\u2026"
lastmod: '2024-03-13T22:44:58.252037-06:00'
model: gpt-4-0125-preview
summary: "En Swift, il n'y a pas de prise en charge native pour l'analyse directe\
  \ des fichiers CSV, mais vous pouvez g\xE9rer les donn\xE9es CSV en utilisant les\
  \ m\xE9thodes `String` pour diviser le contenu, ou en tirant parti de biblioth\xE8\
  ques tierces telles que SwiftCSV pour une approche plus rationalis\xE9e."
title: Travailler avec CSV
weight: 37
---

## Comment faire :
En Swift, il n'y a pas de prise en charge native pour l'analyse directe des fichiers CSV, mais vous pouvez gérer les données CSV en utilisant les méthodes `String` pour diviser le contenu, ou en tirant parti de bibliothèques tierces telles que SwiftCSV pour une approche plus rationalisée. Voici les deux méthodes :

### Analyse Manuelle sans Bibliothèques Externes
```swift
// Considérez une simple chaîne CSV
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Divisez la chaîne CSV en lignes
let rows = csvString.components(separatedBy: "\n")

// Extrait les clés de la première ligne
let keys = rows.first?.components(separatedBy: ",")

// Itérez sur les lignes en commençant par la deuxième
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// Exemple de sortie
print(result)
// Affiche : [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
Cette approche est simple mais manque de robustesse, surtout avec les fichiers CSV contenant des cas particuliers comme des virgules dans les valeurs, des sauts de ligne dans les champs, etc.

### Utilisation de la Bibliothèque SwiftCSV
Tout d'abord, ajoutez SwiftCSV à votre projet en l'incluant dans vos dépendances `Package.swift` :
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Puis, importez et utilisez-le comme suit :
```swift
import SwiftCSV

// Supposons que `csvString` est défini comme ci-dessus

// Créez un objet CSV
if let csv = try? CSV(string: csvString) {
    // Accédez aux lignes sous forme de dictionnaires
    let rows = csv.namedRows
    
    // Exemple de sortie
    print(rows)
    // Affiche : [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV simplifie l'analyse en traitant automatiquement les subtilités comme les virgules encapsulées, les sauts de ligne dans les champs et le codage des caractères. Cependant, n'oubliez pas de gérer les erreurs possibles dans des applications réelles, surtout lorsque vous traitez avec des sources de données externes.
