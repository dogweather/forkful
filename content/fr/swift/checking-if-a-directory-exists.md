---
title:    "Swift: Vérifier l'existence d'un répertoire"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut y avoir plusieurs raisons pour vouloir vérifier si un répertoire existe en programmation Swift. Par exemple, cela peut être utile pour vérifier si un chemin d'accès existe avant de l'utiliser pour créer un fichier ou pour éviter les erreurs lors du chargement de données à partir d'autres sources.

## Comment faire

Pour vérifier si un répertoire existe en Swift, nous pouvons utiliser la méthode `FileManager.default.fileExists` en passant le chemin d'accès du répertoire en tant qu'argument. Voici un exemple de code :

```Swift
let directoryPath = "/Users/UserName/Documents/ExampleFolder"
let fileManager = FileManager.default
if fileManager.fileExists(atPath: directoryPath) {
    print("Le répertoire existe")
} else {
    print("Le répertoire n'existe pas")
}
```

Si le répertoire existe, le programme affichera "Le répertoire existe". Sinon, il affichera "Le répertoire n'existe pas".

## Plongeons plus en profondeur

Lorsque nous utilisons la méthode `fileExists`, il est important de noter que cela peut également renvoyer `true` si le chemin d'accès passé en argument correspond à un fichier. Pour s'assurer que nous vérifions bien l'existence d'un répertoire, nous pouvons ajouter une condition supplémentaire pour vérifier si le chemin d'accès correspond à un répertoire en utilisant la méthode `isDirectory` de `FileManager`. Voici un exemple de code mis à jour :

```Swift
let directoryPath = "/Users/UserName/Documents/ExampleFolder"
let fileManager = FileManager.default
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Le répertoire existe")
    } else {
        print("Le répertoire n'existe pas")
    }
} else {
    print("Le répertoire n'existe pas")
}
```

Cette fois, si le chemin d'accès passé en argument correspond à un fichier, le programme affichera "Le répertoire n'existe pas" même s'il existe un fichier à cet emplacement.

## Voir aussi

- [Documentation officielle Apple sur la méthode `fileExists`](https://developer.apple.com/documentation/foundation/filemanager/1413421-fileexists)
- [Tutoriel sur la gestion des fichiers et répertoires en Swift](https://medium.com/flawless-app-stories/ios-filemanager-explained-delete-copy-and-move-files-and-directories-7e9fcbb3d267)
- [Guide de la programmation en Swift](https://developer.apple.com/documentation/swift)