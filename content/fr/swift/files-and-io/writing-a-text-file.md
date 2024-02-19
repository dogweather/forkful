---
aliases:
- /fr/swift/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:35.075063-07:00
description: "\xC9crire un fichier texte en Swift vous permet de stocker de mani\xE8\
  re persistante des donn\xE9es de cha\xEEne de caract\xE8res dans le syst\xE8me de\
  \ fichiers, ce qui est\u2026"
lastmod: 2024-02-18 23:09:09.229247
model: gpt-4-0125-preview
summary: "\xC9crire un fichier texte en Swift vous permet de stocker de mani\xE8re\
  \ persistante des donn\xE9es de cha\xEEne de caract\xE8res dans le syst\xE8me de\
  \ fichiers, ce qui est\u2026"
title: "R\xE9diger un fichier texte"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire un fichier texte en Swift vous permet de stocker de manière persistante des données de chaîne de caractères dans le système de fichiers, ce qui est essentiel pour des tâches telles que la sauvegarde des paramètres de configuration, des données utilisateur ou des journaux. Les programmeurs font souvent cela pour maintenir les données entre les lancements d'applications, partager des données entre différentes parties d'une application, ou exporter des données pour être utilisées par d'autres programmes.

## Comment faire :

### Utilisation de la bibliothèque standard Swift

La bibliothèque standard de Swift inclut tous les outils nécessaires pour écrire des fichiers texte. Voici une approche de base :

```swift
import Foundation

let content = "Bonjour, lecteurs de Wired ! Apprendre Swift est amusant."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/exemple.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("Fichier écrit avec succès")
} catch let error as NSError {
    print("Échec de l'écriture vers l'URL : \(fileName), Erreur : " + error.localizedDescription)
}
```

Ce morceau de code écrit une chaîne de caractères dans un fichier nommé `exemple.txt` dans le répertoire des documents. Il gère les erreurs potentielles en utilisant la gestion des erreurs do-try-catch de Swift.

### Utilisation de FileManager pour plus de contrôle

Pour plus de contrôle sur les attributs des fichiers ou pour vérifier si le fichier existe déjà, `FileManager` peut être utilisé :

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("exemple.txt")
    let content = "Explorer Swift pour la gestion de fichiers est éclairant."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("Le fichier existe déjà")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("Fichier créé et écrit avec succès")
        } catch {
            print("Erreur lors de l'écriture du fichier : \(error)")
        }
    }
}
```

### Utilisation de bibliothèques tierces

Une bibliothèque tierce populaire pour les opérations sur les systèmes de fichiers en Swift est `Files` de John Sundell :

D'abord, ajoutez Files à votre projet, généralement via le Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "VotreNomDePaquet",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "VotreNomDeCible",
            dependencies: ["Files"]),
    ]
)
```

Ensuite, utilisez-le pour écrire dans un fichier :

```swift
import Files

do {
    let file = try File(path: "/chemin/vers/votre/dossier/exemple.txt")
    try file.write(string: "Swift et la bibliothèque Files forment une combinaison puissante.")
    print("Fichier écrit avec succès en utilisant la bibliothèque Files.")
} catch {
    print("Une erreur s'est produite : \(error)")
}
```

Avec la bibliothèque `Files`, la gestion des fichiers devient plus simple, vous permettant de vous concentrer sur la logique métier de votre application plutôt que sur les détails de la gestion des fichiers.
