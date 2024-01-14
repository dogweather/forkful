---
title:    "Swift: Ecrire un fichier texte"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Swift ?

L'écriture de fichiers texte est une compétence essentielle pour tout programmeur Swift. Cela permet de stocker des données importantes dans un format lisible et facile à manipuler. Que vous souhaitiez enregistrer des paramètres, des résultats ou des entrées d'utilisateur, écrire un fichier texte peut être la solution parfaite pour garder vos données organisées. Dans cet article, nous allons vous montrer comment écrire un fichier texte en utilisant Swift.

## Comment faire ?

Pour écrire un fichier texte en Swift, nous devons utiliser la classe `FileHandle`. Voici un exemple de code qui écrit une chaîne de caractères dans un fichier texte :

```
let fileManager = FileManager.default
let path = fileManager.currentDirectoryPath // obtient le chemin du dossier courant
let myText = "Bonjour, le monde !" // le texte que nous voulons écrire dans le fichier
let fileName = "monfichier.txt" // nom du fichier texte que nous allons créer

// Création du fichier et écriture de la chaîne de caractères
if let file = FileHandle(forWritingAtPath: path + "/" + fileName) {
  file.write(myText.data(using: .utf8)) // convertit la chaîne de caractères en données avant de l'écrire dans le fichier
}

print("Le fichier texte a été créé et le texte a été écrit avec succès.")
```

Avec ce code, nous pouvons créer un fichier texte appelé "monfichier.txt" dans le dossier courant et y écrire la chaîne de caractères "Bonjour, le monde !". En utilisant la classe `FileHandle`, nous pouvons facilement écrire des données dans un fichier texte en utilisant le codage de caractères UTF-8.

## Plongée en profondeur

Il est également possible de créer un fichier texte en utilisant la classe `OutputStream`. Cette méthode offre plus de flexibilité en termes de manipulation et de formatage des données avant de les écrire dans le fichier. Voici un exemple de code pour créer un fichier texte en utilisant `OutputStream` :

```
let fileManager = FileManager.default
let path = fileManager.currentDirectoryPath
let fileName = "monfichier.txt"

// Création et ouverture du flux de sortie
if let outputStream = OutputStream(toFileAtPath: path + "/" + fileName, append: false) {
  outputStream.open()
  let line = "Bonjour, le monde !"
  let data = line.data(using: .utf8) // convertit la chaîne de caractères en données
  let bytes = data!.withUnsafeBytes {
      [UInt8](UnsafeBufferPointer(start: $0, count: data!.count))
  }
  outputStream.write(bytes, maxLength: bytes.count) // écrit les données dans le fichier en utilisant le flux de sortie
}
```

En utilisant `OutputStream`, nous pouvons écrire des données dans le fichier en utilisant différentes méthodes telles que `write()` et `write(_:maxLength:)`. Cela nous donne un contrôle plus précis sur la façon dont les données sont écrites dans le fichier texte.

## Voir aussi

Pour en savoir plus sur l'écriture de fichiers texte en Swift, consultez les ressources suivantes :

- [Documentation officielle sur FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Documentation officielle sur OutputStream](https://developer.apple.com/documentation/foundation/outputstream)
- [Tutoriel vidéo sur l'écriture de fichiers texte en Swift](https://www.youtube.com/watch?v=wA3pw-2hyGw)

Avec ces ressources, vous devriez être en mesure d'écrire des fichiers texte en utilisant Swift et de les manipuler selon vos besoins. Merci d'avoir lu cet article !