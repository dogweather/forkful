---
title:                "Swift: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Swift, il peut être utile de vérifier si un répertoire existe avant de continuer avec une certaine action. Cela peut éviter les erreurs et les plantages indésirables dans votre code.

## Comment faire

Il existe plusieurs façons de vérifier si un répertoire existe en utilisant Swift. Voici quelques exemples de code à utiliser:

```
let fileManager = FileManager.default
let directoryURL = URL(fileURLWithPath: "/path/to/directory")

// Méthode 1: Utiliser la méthode `fileExists` du FileManager
if fileManager.fileExists(atPath: directoryURL.path) {
    print("Le répertoire existe")
} else {
    print("Le répertoire n'existe pas")
}

// Méthode 2: Utiliser la méthode `isReadableFile` du FileManager
if let isDirectory = try? directoryURL.resourceValues(forKeys: [.isReadableKey]).isDirectory, isDirectory {
    print("Le répertoire existe")
} else {
    print("Le répertoire n'existe pas")
}
```

Les deux méthodes ci-dessus utilisent le `FileManager` et le `URL` pour vérifier si le répertoire existe. Vous pouvez également utiliser les mêmes méthodes pour vérifier l'existence de fichiers.

## Plongée en profondeur

La vérification de l'existence d'un répertoire peut sembler simple, mais il y a quelques points à garder à l'esprit. Tout d'abord, si vous travaillez avec des répertoires distants ou sur un réseau, vous devrez peut-être ajouter un délai de réseautage pour attendre que l'opération se termine. Deuxièmement, si vous avez besoin de créer un nouveau répertoire, vous devrez d'abord vérifier s'il existe déjà afin d'éviter les collisions.

En général, il est bon d'utiliser `FileManager` pour effectuer des opérations sur les fichiers et les répertoires, car il gère automatiquement les erreurs et les permissions.

## Voir aussi

- Pour plus d'informations sur la manipulation de fichiers et de répertoires en Swift, consultez [la documentation officielle d'Apple](https://developer.apple.com/documentation/foundation/filemanager).
- Pour des astuces et des conseils utiles sur la programmation Swift, visitez [le blog SwiftLee](https://www.swiftlee.io/).
- Si vous souhaitez en savoir plus sur la programmation en général, [Codecademy](https://www.codecademy.com/) propose des cours gratuits et interactifs pour les débutants.