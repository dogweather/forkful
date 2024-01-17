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

# Qu'est-ce que c'est et pourquoi le faire?
La lecture d'un fichier texte est un moyen pour les programmeurs de récupérer des données textuelles qui sont stockées dans un fichier sur leur ordinateur. Les programmeurs peuvent faire cela pour accéder à des informations statiques ou pour traiter des fichiers à grande échelle.

# Comment faire:
Voici deux façons de lire un fichier texte en Swift:

**1. Utilisation de la méthode String(contentsOf:encoding)**
```swift
let filePath = Bundle.main.path(forResource: "monFichier", ofType: "txt")
let fileContent = try! String(contentsOfFile: filePath!, encoding: .utf8)
print(fileContent)
```
Sortie:
> Contenu de monFichier.txt

**2. Lecture ligne par ligne en utilisant la méthode readLine()**
```swift
var path = "chemin/vers/monFichier.txt"
let lines = try String(contentsOfFile: path, encoding: .utf8).split(separator: "\n")
for line in lines {
  print(line)
}
```
Sortie:
> Contenu de chaque ligne de monFichier.txt

# Plongeons plus en profondeur:
**1. Contexte historique:**
Lecture des fichiers texte est une fonctionnalité de base offerte depuis les premiers langages de programmation. Auparavant, cela nécessitait une implémentation manuelle de code pour lire et traiter les fichiers.

**2. Alternatives:**
Il existe d'autres moyens de récupérer des données à partir de fichiers, tels que l'utilisation de bases de données ou de services de stockage cloud. Cependant, la lecture de fichiers texte est toujours un moyen simple et efficace pour les programmeurs de récupérer des données sur leur ordinateur.

**3. Détails de mise en œuvre:**
En utilisant la méthode String(contentsOf:encoding), il est important de spécifier le chemin complet du fichier, sinon cela peut entraîner des erreurs. De plus, en lisant de gros fichiers, il est préférable d'utiliser la méthode readLine() pour éviter de charger tout le contenu en mémoire à la fois.

# Voir aussi:
- [Documentation Apple pour String(contentsOf:encoding)](https://developer.apple.com/documentation/foundation/string/1412575-contents)
- [Documentation Apple pour readLine()](https://developer.apple.com/documentation/swift/1641190-readline)