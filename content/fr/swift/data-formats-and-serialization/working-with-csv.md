---
aliases:
- /fr/swift/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:43.449643-07:00
description: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique l'analyse et la g\xE9n\xE9ration de donn\xE9es structur\xE9es \xE0 partir\
  \ de fichiers texte\u2026"
lastmod: 2024-02-18 23:09:09.233923
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique l'analyse et la g\xE9n\xE9ration de donn\xE9es structur\xE9es \xE0 partir\
  \ de fichiers texte\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) implique l'analyse et la génération de données structurées à partir de fichiers texte où chaque ligne représente un enregistrement et chaque enregistrement se compose de champs séparés par des virgules. Les programmeurs s'engagent souvent dans cette activité pour importer, exporter et manipuler facilement des données tabulaires en utilisant un format largement pris en charge sur différentes plateformes et langages de programmation, en raison de sa simplicité et de son format lisible par l'homme.

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
