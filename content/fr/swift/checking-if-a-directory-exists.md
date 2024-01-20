---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:58:55.152419-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Vérifier l'existence d'un répertoire assure que votre application n'essaie pas d'accéder à un emplacement qui n'existe pas, ce qui évite les erreurs. Les programmeurs font cela pour confirmer que tout est en place avant de lire ou d'écrire des données.

## Comment faire :
Pour vérifier l'existence d'un répertoire en Swift, on utilise `FileManager` et sa méthode `fileExists(atPath:)`. Voici un petit exemple :

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/chemin/vers/le/repertoire"

if fileManager.fileExists(atPath: path) {
    print("Le répertoire existe.")
} else {
    print("Le répertoire n'existe pas.")
}
```

Si le répertoire existe, vous verrez : 
```
Le répertoire existe.
```

Sinon, le résultat sera :
```
Le répertoire n'existe pas.
```

## Immersion :
Historiquement, la vérification de l'existence d'un fichier ou d'un répertoire est une opération de base dans de nombreux systèmes d'exploitation. En Swift, `FileManager` gère cette tâche. De plus, il faut faire attention à ne pas confondre un fichier avec un répertoire ; pour cela, on peut utiliser `fileExists(atPath:isDirectory:)` que `FileManager` fournit aussi :

```Swift
var isDir : ObjCBool = false
if fileManager.fileExists(atPath: path, isDirectory: &isDir) {
    if isDir.boolValue {
        print("C'est un répertoire.")
    } else {
        print("C'est un fichier.")
    }
}
```

Alternativement, on peut utiliser `attributesOfItem(atPath:)` pour obtenir plus d'informations sur l'élément en question, ou `contentsOfDirectory(atPath:)` pour lister les fichiers dans un répertoire. En choisissant la bonne méthode, on optimise la performance et l'exactitude de notre programme.

## Voir également :
- Documentation Apple sur `FileManager` : [Gestionnaire de fichiers](https://developer.apple.com/documentation/foundation/filemanager)
- Guide sur la manipulation des fichiers et dossiers en Swift : [Apple Developer - Fichiers et Dossiers](https://developer.apple.com/documentation/foundation/file_system)