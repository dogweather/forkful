---
title:                "Lecture d'un fichier texte"
html_title:           "Swift: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Tu écris du code, et parfois tu as besoin de lire un fichier texte. Peut-être pour sauvegarder des données, pour lire une configuration, ou pour récupérer des informations. Dans cet article, tu apprendras comment lire un fichier texte en utilisant Swift.

## Comment faire
La première chose à faire est de récupérer le chemin du fichier que tu veux lire. Tu peux le faire en utilisant la classe `URL` de Swift et en lui donnant le chemin absolu ou relatif du fichier. Puis, tu peux utiliser la méthode `String(contentsOf:encoding:)` pour lire le contenu du fichier et le stocker dans une variable.

````Swift
let filePath = URL(fileURLWithPath: "path/to/file.txt")
do {
    let fileContent = try String(contentsOf: filePath, encoding: .utf8)
    print(fileContent)
} catch {
    print("Error while reading file: \(error)")
}
````
Lorsque tu lances ce code, tu devrais voir le contenu du fichier s'afficher dans la console. Assure-toi que le fichier existe et que le chemin que tu as fourni est correct.

Ensuite, tu peux utiliser la méthode `components(separatedBy:)` pour séparer le contenu du fichier en différentes parties, en utilisant un caractère ou une chaîne comme séparateur. Par exemple, si ton fichier contient plusieurs noms séparés par des virgules, tu peux utiliser `fileContent.components(separatedBy: ",")` pour obtenir un tableau de noms.

## Deep Dive
En utilisant `String(contentsOf:encoding:)`, tu obtiendras le contenu du fichier sous forme de chaîne de caractères. Mais si tu veux traiter le contenu du fichier ligne par ligne, tu peux utiliser la méthode `components(separatedBy: CharacterSet.newlines)` qui sépare le contenu en fonction des sauts de lignes.

De plus, tu peux spécifier l'encodage du fichier en utilisant le paramètre `encoding`. Par défaut, il est configuré sur `.utf8`, mais tu peux également utiliser d'autres encodages comme `.ascii` ou `.utf16` en fonction des besoins de ton fichier.

## Voir aussi
- [La documentation officielle de Swift sur la classe URL](https://developer.apple.com/documentation/foundation/url)
- [Un tutoriel sur la lecture et l'écriture de fichiers en Swift](https://www.raywenderlich.com/1951-swift-tutorial-part-3-tuples-protocols-delegates-and-table-views#reading-writing-to-a-file)
- [Un article sur les différentes méthodes pour lire et écrire des fichiers en Swift](https://medium.com/@khunshan/swift-4-0-working-with-text-files-in-swift-c5ab61e778af)