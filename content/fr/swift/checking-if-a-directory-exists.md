---
title:    "Swift: Vérifier si un répertoire existe"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous développez une application mobile ou un logiciel, vous devez souvent interagir avec le système de fichiers de l'appareil. Il peut être utile de vérifier si un dossier existe avant de tenter d'y accéder ou d'y enregistrer des fichiers. Cela permet d'éviter les erreurs et d'assurer un bon fonctionnement de votre application.

## Comment faire

Pour vérifier si un dossier existe en Swift, vous pouvez utiliser la méthode `fileExists(atPath:)` de la classe `FileManager`. Cette méthode prend en paramètre un chemin d'accès (string) vers le dossier que vous souhaitez vérifier et renvoie un booléen indiquant si le dossier existe ou non.

```
let manager = FileManager.default
let documentDirectory = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0]
let folderPath = documentDirectory.appending("/MonDossier")

if manager.fileExists(atPath: folderPath) {
    print("Le dossier existe")
} else {
    print("Le dossier n'existe pas")
}
```

Ce code utilise `NSSearchPathForDirectoriesInDomains` pour obtenir le chemin d'accès au dossier de documents de l'utilisateur, puis ajoute "/MonDossier" pour créer un chemin d'accès vers un dossier spécifique. Ensuite, il utilise `fileExists(atPath:)` pour vérifier si ce dossier existe et affiche le résultat dans la console.

## Approfondissement

Pour une vérification plus précise, vous pouvez utiliser la méthode `fileExists(atPath:isDirectory:)` qui prend en compte le type de fichier. Si vous utilisez `fileExists(atPath:)`, un fichier et un dossier avec le même nom seront considérés comme existants.

Il est également possible de vérifier si un dossier existe à un chemin spécifique en utilisant `fileExists(atPath:)` sur le chemin complet. Par exemple :

```
let manager = FileManager.default
let temporaryDirectory = NSTemporaryDirectory()
let folderPath = temporaryDirectory.appending("MonDossier")

if manager.fileExists(atPath: folderPath, isDirectory: nil) {
    print("Le dossier existe")
} else {
    print("Le dossier n'existe pas")
}
```

En utilisant `temporaryDirectory`, le code vérifie si le dossier "MonDossier" existe dans le dossier temporaire de l'appareil.

## Voir aussi

- [Documentation officielle de Swift](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID444)
- [Article sur la gestion des fichiers en Swift](https://www.hackingwithswift.com/read/0/10/basic-ios-workflows-reading-and-writing-files)
- [Tutoriel vidéo sur la gestion des fichiers en Swift](https://www.youtube.com/watch?v=QMxKKzQqsW8)